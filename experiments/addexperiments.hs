import System.Environment (getArgs, getProgName)
import Database.HDBC
import Database.HDBC.Sqlite3
import HDBUtils
import Text.ParserCombinators.Parsec
import Data.Functor ((<$>))
import ExperimentDescription
import ParsecUtils
import qualified DatabaseStructure as DS
  
appendError :: String -> Either String a -> Either String a
appendError _ x@(Right _) = x
appendError str (Left err) = Left (str ++ err)
  
parsePositiveInteger :: String -> Either String Int
parsePositiveInteger str = do
  let parseResult = parse natural "" str
  case parseResult of
    Left parseError -> Left $ show parseError
    Right n -> return n

validateStrategy :: Strategy -> Either String Strategy
validateStrategy Perfect = Right Perfect
validateStrategy strat@(UCT numIterations)
  | numIterations > 0 = Right strat
  | otherwise         = Left "UCT strategy must have at least one iteration"

sanitizeExperimentArgs :: [String] -> Either String Experiment
sanitizeExperimentArgs args = 
  case args of
    firstStratStr:secondStratStr:optionalArgs -> do
      firstStrat <- appendError "first argument: " $ parseStrategy firstStratStr >>= validateStrategy
      secondStrat <- appendError "second argument: " $ parseStrategy secondStratStr >>= validateStrategy
      case (firstStrat, secondStrat) of
        (Perfect, Perfect) -> return $ Deterministic Perfect Perfect
        (fstStrat, sndStrat) -> do
          case optionalArgs of
            [numPlaysStr] -> do
              numPlays <- appendError "third argument: " $ parsePositiveInteger numPlaysStr
              return $ Stochastic fstStrat sndStrat numPlays
            _ -> Left "when specifying stochastic strategies, you must also specify the number of games to play"
    _ -> Left "wrong number of arguments"

addExperiment :: IConnection c => c -> Experiment -> IO ()
addExperiment connection experiment@(Deterministic firstStrategy secondStrategy) = do
  let tableName = DS.tableName $ DS.resultTableMetadata experiment 
  insertStatement <- prepare connection $ concat ["INSERT OR IGNORE INTO experiments (experiment, strategy_first, strategy_second)",
                                                " VALUES (?, ?, ?)"]
  execute insertStatement [toSql $ tableName,
                           toSql $ show firstStrategy,
                           toSql $ show secondStrategy]
  commit connection  
addExperiment connection experiment@(Stochastic firstStrategy secondStrategy numPlays) = do
  let tableName = DS.tableName $ DS.resultTableMetadata experiment 
  insertStatement <- prepare connection $ concat ["INSERT OR IGNORE INTO experiments (experiment, strategy_first, strategy_second, num_plays)",
                                                
                                                " VALUES (?, ?, ?, ?)" ]
  execute insertStatement [toSql $ tableName,
                           toSql $ show firstStrategy,
                           toSql $ show secondStrategy,
                           toSql $ numPlays]
  commit connection

-- Make a connection with an experiment database, with the intent of adding some specific experiment.
-- This function will make sure that the required tables exist, or it will create them.
-- If it fails to do both of these tasks, it will fail with an error message.
connectExperimentDatabase :: String -> Experiment -> IO (Either String (Connection, Experiment))
connectExperimentDatabase filename experiment = do
  result <- (connectSqlite3 filename >>= requireExperimentTable >>= either (return . Left) (requireResultTable experiment))
  case result of
    Left error -> do
      return $ Left $ "connecting to experiment database: " ++ error
    Right connection -> do
      return $ Right (connection, experiment)
  where
    requireExperimentTable :: IConnection c => c -> IO (Either String c) 
    requireExperimentTable connection = do
      result <- requireTable DS.experimentTableMetadata connection
      case result of 
        Left error -> return $ Left $ "failed to require experiment table: " ++ error
        Right connection -> return $ Right connection
    requireResultTable :: IConnection c => Experiment -> c -> IO (Either String c)
    requireResultTable experiment@(Deterministic _ _) connection = do
      result <- requireTable (DS.resultTableMetadata experiment) connection
      case result of
        Left error -> return $ Left $ "failed to require result table: " ++ error
        Right connection -> return $ Right connection
    requireResultTable (Stochastic _ _ _) connection = do
      result <- requireTable (DS.resultTableMetadata experiment) connection
      case result of
        Left error -> return $ Left $  "failed to require result table: " ++ error
        Right connection -> return $ Right connection

main = do
  args <- getArgs
  case args of
    filename:experimentArgs -> do
      let experimentParseResult = sanitizeExperimentArgs experimentArgs
      case experimentParseResult of
        Left error -> do
          reportError $ "failed to parse experiment arguments: " ++ error
        Right experiment -> do
          connectionResult <- connectExperimentDatabase filename experiment
          case connectionResult of
            Left error -> do
              reportError $ "failed to connect to the database: " ++ error
            Right (connection, experiment) -> do
              addExperiment connection experiment
              disconnect connection
    _ -> reportError $ unlines ["You need to specify a filename for the database where you wish to add the experiments.",
                                "The file will be created if it doesn't exist."]
  where
    usageMessage programName = 
      unlines ["usage: ",
               concat [programName, " filename first_strategy second_strategy [num_plays]"],
               "filename: The filename of the database you wish to insert the experiments into",
               "first_strategy: Strategy for first player. (e.g. \"Perfect\" for perfect strategy, or \"UCT(100)\" for UCT with 100 iterations. ",
               "second_strategy: strategy for second player.",
               "num_plays: Number of times to play a given game. Only makes sense if you specify at least one stochastic strategy."
              ]    
    reportError error = do
      programName <- getProgName      
      putStrLn $ "error: " ++ error
      putStr $ usageMessage programName