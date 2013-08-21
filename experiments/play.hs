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

chunksOf n [] = []
chunksOf n xs = (take n xs):(chunksOf n $ drop n xs)


runExperiment :: IConnection c => c -> Int -> ED.Experiment -> IO ()
runExperiment connection batchSize experiment@(ED.Stochastic (ED.UCT _) (ED.UCT _) _) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                ", num_iterations_first, num_iterations_second, num_plays", -- experiment columns
                                ", numvertices, representation",                            -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      updateStatement = concat ["UPDATE ", tableName,
                                " SET num_first_wins = ?, num_second_wins = ?, num_neither_wins = ?",
                                " WHERE hypergraph = ? AND num_iterations_first = ? AND num_iterations_second = ? AND num_plays = ?"
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
      convertResult (hypergraph, numIterationsFirst, numIterationsSecond, numPlays, result) = 
        [toSql $ numFirstWins result, toSql $ numSecondWins result, toSql $ numNeitherWins result, toSql hypergraph, toSql numIterationsFirst, toSql numIterationsSecond, toSql numPlays]
  experimentOrErrors <- map toExperiment <$> quickQuery connection selectStatement []
  let (errors, experiments) = (lefts experimentOrErrors, rights experimentOrErrors)
  case errors of
    [] -> do
      update <- prepare connection updateStatement
      let jobChunks = chunksOf batchSize experimentOrErrors 
          processOneJob (hypergraph, game, experiment@(ED.Stochastic (ED.UCT numIterationsFirst) (ED.UCT numIterationsSecond) numPlays)) = do
            result <- processJob (PoGa.Game $ PoGa.unexploredMetaDataNode game)
                      (PoGa.mctsStrategyFirst . ED.numIterations . ED.firstStrategy $ experiment)
                      (PoGa.mctsStrategySecond . ED.numIterations . ED.secondStrategy $ experiment)
                      (ED.numPlays experiment)
            return (hypergraph, numIterationsFirst, numIterationsSecond, numPlays, result)
      sequence_ [do batch <- mapM processOneJob $ rights jobChunk
                    executeMany update (map convertResult batch)
                    commit connection
                    case lefts jobChunk of
                      [] -> return ()
                      errors -> do
                        putStrLn "errors:"                        
                        print $ unlines $ errors
                | jobChunk <- jobChunks]
    _ -> putStrLn $ unlines errors

runExperiment connection batchSize experiment@(ED.Stochastic ED.Perfect (ED.UCT _) _) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                ", num_iterations_second, num_plays", -- experiment columns
                                ", numvertices, representation",      -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      updateStatement = concat ["UPDATE ", tableName,
                                " SET num_first_wins = ?, num_second_wins = ?, num_neither_wins = ?",
                                " WHERE hypergraph = ? AND num_iterations_second = ? AND num_plays = ?"
                                ]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numIterationsSecondSql, numPlaysSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numIterationsSecond <- safeSqlConvert numIterationsSecondSql
        numPlays <- safeSqlConvert numPlaysSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Stochastic ED.Perfect (ED.UCT numIterationsSecond) numPlays)
      convertResult (hypergraph, numIterationsSecond, numPlays, result) = 
        [toSql $ numFirstWins result, toSql $ numSecondWins result, toSql $ numNeitherWins result, toSql hypergraph, toSql numIterationsSecond, toSql numPlays]
  experimentOrErrors <- map toExperiment <$> quickQuery connection selectStatement []
  let (errors, experiments) = (lefts experimentOrErrors, rights experimentOrErrors)
  case errors of
    [] -> do
      update <- prepare connection updateStatement
      let jobChunks = chunksOf batchSize experimentOrErrors 
          processOneJob (hypergraph, game, experiment@(ED.Stochastic ED.Perfect (ED.UCT numIterationsSecond) numPlays)) = do
            result <- processJob (PoGa.Game $ PoGa.unexploredMetaDataNode game)
                      (PoGa.perfectStrategyFirst)
                      (PoGa.mctsStrategyFirst . ED.numIterations . ED.secondStrategy $ experiment)
                      (ED.numPlays experiment)
            return (hypergraph, numIterationsSecond, numPlays, result)
      sequence_ [do batch <- mapM processOneJob $ rights jobChunk
                    executeMany update (map convertResult batch)
                    commit connection
                    case lefts jobChunk of
                      [] -> return ()
                      errors -> do
                        putStrLn "errors:"                        
                        print $ unlines $ errors
                | jobChunk <- jobChunks]
    _ -> putStrLn $ unlines errors

runExperiment connection batchSize experiment@(ED.Stochastic (ED.UCT _) ED.Perfect _) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph",
                                ", num_iterations_first, num_plays", -- experiment columns
                                ", numvertices, representation",     -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE num_first_wins IS NULL AND num_second_wins IS NULL AND num_neither_wins IS NULL"]
      updateStatement = concat ["UPDATE ", tableName,
                                " SET num_first_wins = ?, num_second_wins = ?, num_neither_wins = ?",
                                " WHERE hypergraph = ? AND num_iterations_first = ? AND num_plays = ?"
                                ]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numIterationsFirstSql, numPlaysSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numIterationsFirst <- safeSqlConvert numIterationsFirstSql
        numPlays <- safeSqlConvert numPlaysSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Stochastic (ED.UCT numIterationsFirst) ED.Perfect numPlays)
      convertResult (hypergraph, numIterationsFirst, numPlays, result) = 
        [toSql $ numFirstWins result, toSql $ numSecondWins result, toSql $ numNeitherWins result, toSql hypergraph, toSql numIterationsFirst, toSql numPlays]
  experimentOrErrors <- map toExperiment <$> quickQuery connection selectStatement []
  update <- prepare connection updateStatement
  let jobChunks = chunksOf batchSize experimentOrErrors
      processOneJob (hypergraph, game, experiment@(ED.Stochastic (ED.UCT numIterationsFirst) ED.Perfect numPlays )) = do
        result <- processJob (PoGa.Game $ PoGa.unexploredMetaDataNode game)
                  (PoGa.mctsStrategyFirst . ED.numIterations . ED.firstStrategy $ experiment)
                  (PoGa.perfectStrategySecond)
                  (ED.numPlays experiment)
        return (hypergraph, numIterationsFirst, numPlays, result)      
  sequence_ [do batch <- mapM processOneJob $ rights jobChunk
                executeMany update (map convertResult batch)
                commit connection
                -- print out errors if any
                case lefts jobChunk of
                  [] -> return ()
                  errors -> do
                    putStrLn "errors:"
                    print $ unlines $ errors
            | jobChunk <- jobChunks]

runExperiment connection batchSize experiment@(ED.Deterministic ED.Perfect ED.Perfect) = do
  let tableName = DS.experimentResultTableName experiment
      selectStatement = concat ["SELECT hypergraph, numvertices, representation",  -- hypergraph columns
                                " FROM ", tableName, " NATURAL JOIN ", DS.tableName DS.hypergraphTableMetadata,
                                " WHERE winner IS NULL"]
      updateStatement = concat ["UPDATE ", tableName,
                                " SET winner = ?",
                                " WHERE hypergraph = ?"
                                ]
      toExperiment :: [SqlValue] -> Either String (String, PoGa.SetGame Int, ED.Experiment)
      toExperiment [hypergraphSql, numVerticesSql, representationSql] = do
        hypergraph <- safeSqlConvert hypergraphSql
        numVertices <- safeSqlConvert numVerticesSql
        representation <- safeSqlConvert representationSql
        return (hypergraph, fromRepresentation numVertices representation, ED.Deterministic ED.Perfect ED.Perfect)
      showWinner (PoGa.Only PoGa.First) = show PoGa.First
      showWinner (PoGa.Only PoGa.Second) = show PoGa.Second
      showWinner (PoGa.Neither) = show PoGa.Neither      
      convertResult (hypergraph, winner) = 
        [toSql $ showWinner $ winner, toSql hypergraph]
  experimentOrErrors <- map toExperiment <$> quickQuery connection selectStatement []
  update <- prepare connection updateStatement
  let jobChunks = chunksOf batchSize experimentOrErrors
      processOneJob (hypergraph, game, experiment) = do
        winner <- PoGa.playGame (PoGa.Game game)
                  (PoGa.perfectStrategyFirst)
                  (PoGa.perfectStrategySecond)
        return (hypergraph, winner)
  sequence_ [do batch <- mapM processOneJob $ rights jobChunk
                executeMany update (map convertResult batch)
                commit connection
                -- print out errors if any
                case lefts jobChunk of
                  [] -> return ()
                  errors -> do
                    putStrLn "errors:"
                    print $ unlines $ errors                
            | jobChunk <- jobChunks]
  
main = do
  programName <- getProgName
  args <- getArgs
  case args of
    [filename] -> do
      playWithArgs filename defaultBatchSize
    [filename, batchSizeString] -> do
      case reads batchSizeString of
        [(batchSize, "")] -> do
          case batchSize > 0 of
            True -> playWithArgs filename batchSize
            False -> do
              putStrLn "argument 'batch size' is negative"
              putStrLn "usage: "
              putStrLn $ usage programName
        _ -> do
          putStrLn $ "second argument: " ++ batchSizeString ++ " ('batch size') is not an integer"
          putStrLn "usage: "
          putStrLn $ usage programName
    _ -> do
      putStrLn "incorrect number of arguments"
      putStrLn "usage: "
      putStrLn $ usage programName
  where
    defaultBatchSize = 500    
    usage programName = programName ++ " [database filename] " ++ "[optionally: batch size (default is: " ++ show defaultBatchSize ++ ")"
    playWithArgs filename batchSize = do
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
                      mapM_ (runExperiment connection batchSize) experiments
                    errors -> do
                      putStrLn "the database is errorneously formatted: "
                      mapM_ putStrLn errors
          disconnect connection
    