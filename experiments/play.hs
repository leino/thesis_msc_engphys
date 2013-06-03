import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getProgName)
import qualified DatabaseStructure as DS
import qualified ExperimentDescription as ED
import HDBUtils
import Data.List (intersperse)
import FromRepresentation (fromRepresentation)
import qualified GameTheory.PoGa as PoGa
import Common
import Control.Applicative ((<$>))
import Data.Either (lefts, rights)
import System.Directory (doesFileExist)

data Result = Result {numFirstWins :: Int, numSecondWins :: Int, numNeitherWins :: Int} deriving Show

processJob :: (Monad m, PoGa.Position p) => PoGa.Game p -> PoGa.Strategy m p -> PoGa.Strategy m p -> Int -> m Result
processJob game firstStrategy secondStrategy numPlays = do
  let accumulate (Result f s n) (PoGa.Only PoGa.First) = Result (f + 1) s n
      accumulate (Result f s n) (PoGa.Only PoGa.Second) = Result f (s + 1) n
      accumulate (Result f s n) PoGa.Neither = Result f s (n + 1)
  results <- PoGa.playTournament numPlays game firstStrategy secondStrategy
  return $ foldl accumulate (Result 0 0 0) results

runExperiment :: IConnection c => c -> ED.Experiment -> IO ()
runExperiment connection experiment@(ED.Stochastic (ED.UCT _) (ED.UCT _) _) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                ", num_iterations_first, num_iterations_second, num_plays", -- experiment columns
                                ", numvertices, representation",                            -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      insertStatement = concat ["REPLACE INTO ", tableName,
                                " (hypergraph, num_first_wins, num_second_wins, num_neither_wins)",
                                " VALUES (?,?,?,?)"
                                ]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numIterationsFirstSql, numIterationsSecondSql, numPlaysSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numIterationsFirst <- safeSqlConvert numIterationsFirstSql
        numIterationsSecond <- safeSqlConvert numIterationsSecondSql
        numPlays <- safeSqlConvert numPlaysSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Stochastic (ED.UCT numIterationsFirst) (ED.UCT numIterationsSecond) numPlays)
      convertResult (hypergraph, result) = 
        [toSql hypergraph, toSql $ numFirstWins result, toSql $ numSecondWins result, toSql $ numNeitherWins result]
  experimentOrErrors <- map toExperiment <$> quickQuery connection selectStatement []
  let (errors, experiments) = (lefts experimentOrErrors, rights experimentOrErrors)
  case errors of
    [] -> do
      insertion <- prepare connection insertStatement
      sequence_ [do result <- processJob (PoGa.Game $ PoGa.Unexplored game)
                                         (PoGa.mctsStrategyFirst . ED.numIterations . ED.firstStrategy $ experiment)
                                         (PoGa.mctsStrategySecond . ED.numIterations . ED.secondStrategy $ experiment)
                                         (ED.numPlays experiment)
                    putStrLn $ concat ["hypergraph: ", hypergraph, ", result: ", show result]
                | (hypergraph, game, experiment) <- experiments]
      -- executeMany insertion (map convertResult results)
    _ -> putStrLn $ unlines errors

runExperiment connection experiment@(ED.Stochastic ED.Perfect (ED.UCT _) _) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                ", num_iterations_second, num_plays", -- experiment columns
                                ", numvertices, representation",      -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numIterationsSecondSql, numPlaysSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numIterationsSecond <- safeSqlConvert numIterationsSecondSql
        numPlays <- safeSqlConvert numPlaysSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Stochastic ED.Perfect (ED.UCT numIterationsSecond) numPlays)
  experiments <- map toExperiment <$> quickQuery connection selectStatement []
  return ()
--  putStrLn $ unlines $ map show experiments

runExperiment connection experiment@(ED.Stochastic (ED.UCT _) ED.Perfect _) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                ", num_iterations_first, num_plays", -- experiment columns
                                ", numvertices, representation",     -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numIterationsFirstSql, numPlaysSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numIterationsFirst <- safeSqlConvert numIterationsFirstSql
        numPlays <- safeSqlConvert numPlaysSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Stochastic (ED.UCT numIterationsFirst) ED.Perfect numPlays)
  experiments <- map toExperiment <$> quickQuery connection selectStatement []
  return ()
--  putStrLn $ unlines $ map show experiments  

runExperiment connection experiment@(ED.Deterministic ED.Perfect ED.Perfect) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                " numvertices, representation",  -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE winner IS NULL"]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Deterministic ED.Perfect ED.Perfect)
  experiments <- map toExperiment <$> quickQuery connection selectStatement []
  return ()
--  putStrLn $ unlines $ map show experiments

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