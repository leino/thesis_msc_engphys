import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getProgName)
import qualified DatabaseStructure as DS
import qualified ExperimentDescription as ED
import HDBUtils
import Data.List (intersperse)
import Common
import Data.Functor ((<$>))
import Data.Either (lefts, rights)
import System.Directory (doesFileExist)

runExperiment :: IConnection c => c -> ED.Experiment -> IO ()
runExperiment connection experiment@(ED.Stochastic (ED.UCT _) (ED.UCT _) _) = do
  let tableName = DS.experimentResultTableName experiment  
      selectStatement = concat ["SELECT hypergraph, num_iterations_first, num_iterations_second, num_plays",
                                " FROM ", tableName,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      toExperiment :: [SqlValue] -> Either String (String, ED.Experiment)
      toExperiment [hypergraphSql, numIterationsFirstSql, numIterationsSecondSql, numPlaysSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numIterationsFirst <- safeSqlConvert numIterationsFirstSql
        numIterationsSecond <- safeSqlConvert numIterationsSecondSql
        numPlays <- safeSqlConvert numPlaysSql
        return (hypergraph, ED.Stochastic (ED.UCT numIterationsFirst) (ED.UCT numIterationsSecond) numPlays)

  putStrLn $ "running unfinished experiments on table" ++ tableName
  results <- quickQuery connection selectStatement []
  putStrLn $ "number of results: " ++ (show $ length results)
  putStrLn $ unlines $ map (show . toExperiment) results

runExperiment connection (ED.Stochastic ED.Perfect (ED.UCT _) _) = return ()
runExperiment connection (ED.Stochastic (ED.UCT _) ED.Perfect _) = return ()
runExperiment connection (ED.Deterministic ED.Perfect ED.Perfect) = return ()
  
main = do
  programName <- getProgName
  args <- getArgs
  case args of
    [filename] -> do
      fileExists <- doesFileExist filename
      case fileExists of
        False -> putStrLn $ "file does not exist: " ++ filename
        True -> do
          connection <- connectSqlite3 filename
          experimentTablePresence <- checkTablePresence DS.experimentTableMetadata connection
          case experimentTablePresence of
            Invalid -> do
              putStrLn $ "error: experiment table has invalid column structure"
            Missing -> do
              putStrLn $ "error: The experiment table is missing. Please add some experiments using the 'addexperiments' command."
            Present -> do
              hypergraphTablePresence <- checkTablePresence DS.hypergraphTableMetadata connection
              case hypergraphTablePresence of
                Invalid -> do
                  putStrLn $ "error: hypergraph table has invalid column structure"
                Missing -> do
                  putStrLn $ "error: The hypergraph table is missing. Please add some hypergraphs using the 'addhypergraphs' command."
                Present -> do          
                  readResults <- readExperiments connection
                  let errors = lefts readResults
                  case errors of
                    [] -> do
                      let experiments = rights readResults
                      mapM_ (runExperiment connection) experiments
                    errors -> do
                      putStrLn "the database is errorneously formatted: "
                      mapM_ putStrLn errors
          disconnect connection
    _ -> do
      putStrLn "incorrect number of arguments"
      putStrLn "usage: "
      putStrLn $ usage programName
  where
    usage programName = programName ++ " [database filename]"