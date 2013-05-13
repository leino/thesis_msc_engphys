{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import HDBUtils
import Data.Functor ((<$>))
import Control.Arrow (left)
import Data.Either (lefts, rights)
import Data.Convertible
import qualified ExperimentDescription as ED
import qualified DatabaseStructure as DS

safeSqlConvert :: Convertible SqlValue a => SqlValue -> Either String a
safeSqlConvert = left prettyConvertError . safeFromSql

readExperiments :: IConnection c => c -> IO [Either String ED.Experiment]
readExperiments connection = do
  let readRow row@[sqlFirstStrat,
                   sqlSecondStrat,
                   sqlNumPlays] = do
        left ((++) $ "row: " ++ show row ++ ": ") $ do -- append the row sql representation to error messages, for context
          firstStrategy <- safeSqlConvert sqlFirstStrat >>= ED.parseStrategy
          secondStrategy <- safeSqlConvert sqlSecondStrat >>= ED.parseStrategy
          case (firstStrategy, secondStrategy) of
            ((ED.Perfect, ED.Perfect)) -> do
              return $ ED.Deterministic {ED.firstStrategy = firstStrategy,
                                         ED.secondStrategy = secondStrategy}
            _ -> do -- we have at least one stochastic strategy, so the experiment is stochastic
              numPlays <- (safeSqlConvert sqlNumPlays)::Either String Int
              case numPlays > 0 of
                False ->
                  Left $ "number of plays specified is not strictly positive"
                True -> 
                  return $ ED.Stochastic {ED.firstStrategy = firstStrategy,
                                          ED.secondStrategy = secondStrategy,
                                          ED.numPlays = numPlays}
            
  map readRow <$> quickQuery connection (concat ["SELECT strategy_first, strategy_second, num_plays FROM ",
                                                 tableName DS.experimentTableMetadata]) []

main = do
  args <- getArgs
  case args of
    [filename] -> do
      databaseExists <- doesFileExist filename
      case databaseExists of
        False -> do
          putStrLn $ "the file " ++ filename ++ " does not exist"
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
                          tableNames = map DS.experimentResultTableName experiments
                      putStr $ unlines $ map show $ experiments
                      putStr $ unlines $ map show $ tableNames
                    _ -> do
                      putStrLn "there are errors in the experiment table: "
                      putStr $ unlines $ errors
                  
          disconnect connection
    [] -> do
      reportError "no database filename given"
    _ -> do
      reportError "wrong number of arguments given: only a single database filename is required"
  where
    usage programName = 
      unlines ["usage: ",
               concat [programName, ": filename"],
               "filename: Database for which you want to plan experiments.",
               "Hypergraphs and experiments should already have been added."]
    reportError error = do
      putStrLn $ "error: " ++ error
      programName <- getProgName
      putStr $ usage programName