import System.Environment (getArgs, getProgName)
import Database.HDBC
import Database.HDBC.Sqlite3
import HDBUtils
import Text.ParserCombinators.Parsec
import Data.Functor ((<$>))

data Strategy = Perfect |
                UCT {numIterations :: Int}
                
data Experiment = Deterministic Strategy Strategy |
                  Stochastic {firstStrategy :: Strategy, secondStrategy :: Strategy, numPlays :: Int}
                deriving Show

instance Show Strategy where
  show Perfect = "Perfect"
  show (UCT n) = concat ["UCT", show n]
  
experimentResultTableName (Deterministic firstStrategy secondStrategy) =
  "results_" ++ show firstStrategy ++ "_" ++ show secondStrategy
  
experimentResultTableName (Stochastic firstStrategy secondStrategy numPlays) = do
  "results_" ++ show firstStrategy ++ "_" ++ show secondStrategy ++ "_" ++ show numPlays ++ "plays"
appendError :: String -> Either String a -> Either String a
appendError _ x@(Right _) = x
appendError str (Left err) = Left (str ++ err)

natural = read <$> many1 digit

parseStrategy :: String -> Either String Strategy
parseStrategy str = do
  let p = choice [string "Perfect" >> return Perfect,
                  string "UCT" >> between (char '(') (char ')') natural >>= return . UCT]
      parseResult = parse p "" str
  case parseResult of
    Left parseError -> Left $ show $ parseError
    Right experiment -> return experiment
    
  
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
  let experimentName = experimentResultTableName experiment
  insertStatement <- prepare connection $ concat ["INSERT INTO experiments (experiment, strategy_first, strategy_second)",
                                                " VALUES (?, ?, ?)"]
  execute insertStatement [toSql experimentName,
                           toSql $ show firstStrategy,
                           toSql $ show secondStrategy]
  commit connection  
addExperiment connection experiment@(Stochastic firstStrategy secondStrategy numPlays) = do
  let experimentName = experimentResultTableName experiment
  insertStatement <- prepare connection $ concat ["INSERT INTO experiments (experiment, strategy_first, strategy_second, num_plays)",
                                                
                                                " VALUES (?, ?, ?, ?)" ]
  execute insertStatement [toSql experimentName,
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
      let tableName = "experiments"
          requiredColumns = 
            let stringDesc = SqlColDesc {colType = SqlUnknownT "string", colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing}
                integerDesc = SqlColDesc {colType = SqlIntegerT, colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing} in
            [("experiment", stringDesc), ("strategy_first", stringDesc), ("strategy_second", stringDesc), ("num_plays", integerDesc)]
          createStatement = concat ["CREATE TABLE ", tableName,
                                    " (experiment STRING PRIMARY KEY NOT NULL,",
                                    " strategy_first STRING NOT NULL,",
                                    " strategy_second STRING NOT NULL,",
                                    " num_plays INTEGER)"]
      result <- requireTable tableName createStatement requiredColumns connection
      case result of 
        Nothing -> return $ Left "failed to require experiment table"
        Just connection -> return $ Right connection
    requireResultTable :: IConnection c => Experiment -> c -> IO (Either String c)
    requireResultTable (Deterministic _ _) connection = do
      let tableName = experimentResultTableName experiment          
          requiredColumns = 
            let stringDesc = SqlColDesc {colType = SqlUnknownT "string", colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing} in
            [("game", stringDesc), ("winner", stringDesc)]
          createStatement = concat ["CREATE TABLE ", tableName,
                                    " (game STRING PRIMARY KEY NOT NULL,",
                                    " winner STRING)"]
      result <- requireTable tableName createStatement requiredColumns connection
      case result of
        Nothing -> return $ Left "failed to require result table"
        Just connection -> return $ Right connection
    requireResultTable (Stochastic _ _ _) connection = do
      let tableName = experimentResultTableName experiment
          requiredColumns = 
            let stringDesc = SqlColDesc {colType = SqlUnknownT "string", colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing}
                integerDesc = SqlColDesc {colType = SqlIntegerT, colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing} in
            [("game", stringDesc), ("num_first_wins", integerDesc), ("num_second_wins", integerDesc), ("num_neither_wins", integerDesc)]
          createStatement = concat ["CREATE TABLE ", tableName,
                                    " (game STRING PRIMARY KEY NOT NULL,",
                                    " num_first_wins INTEGER,",
                                    " num_second_wins INTEGER,",
                                    " num_neither_wins INTEGER)"]
      result <- requireTable tableName createStatement requiredColumns connection
      case result of
        Nothing -> return $ Left "failed to require result table"
        Just connection -> return $ Right connection

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