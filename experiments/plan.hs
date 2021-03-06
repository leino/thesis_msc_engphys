{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import HDBUtils
import Data.Functor ((<$>))
import Data.Either (lefts, rights)
import qualified ExperimentDescription as ED
import qualified DatabaseStructure as DS
import Common (readExperiments)

requireResultTable :: IConnection c => c -> ED.Experiment -> IO (Either String c)
requireResultTable connection experiment@(ED.Deterministic _ _) = do
  result <- requireTable (DS.resultTableMetadata experiment) connection
  case result of
    Left error -> return $ Left $ "failed to require result table: " ++ error
    Right connection -> return $ Right connection
requireResultTable connection experiment@(ED.Stochastic _ _ _) = do
  result <- requireTable (DS.resultTableMetadata experiment) connection
  case result of
    Left error -> return $ Left $  "failed to require result table: " ++ error
    Right connection -> return $ Right connection

appendResults :: IConnection c => c -> ED.Experiment -> IO Integer
appendResults connection experiment@(ED.Deterministic ED.Perfect ED.Perfect) = do
  let resultTableName = DS.experimentResultTableName experiment
  insertStatement <- prepare connection $ concat ["INSERT OR REPLACE INTO ", resultTableName,
                                                  " (hypergraph) ",
                                                  " SELECT hypergraph FROM ",
                                                  DS.tableName DS.hypergraphTableMetadata
                                                 ]
  execute insertStatement []
      
appendResults connection experiment@( ED.Stochastic ED.Perfect (ED.UCT num_iterations) num_plays) = do
  let resultTableName = DS.experimentResultTableName experiment
      temporaryTableName = "tmp_" ++ resultTableName
  -- create a temporary table
  createTemporaryTableStatement <- prepare connection $ concat ["CREATE TEMPORARY TABLE ", temporaryTableName, " (num_iterations_second INTEGER NOT NULL, num_plays INTEGER NOT NULL)"]
  execute createTemporaryTableStatement []  
  -- insert the desired experiment into the temporary table
  insertTempTableStatement <- prepare connection $ concat ["INSERT INTO ", temporaryTableName, " (num_iterations_second, num_plays) VALUES (?, ?)"]
  execute insertTempTableStatement [toSql num_iterations, toSql num_plays]  
  -- make the insertion
  insertStatement <- prepare connection $ concat ["INSERT OR REPLACE INTO ", resultTableName, 
                                                  " (hypergraph, num_iterations_second, num_plays) ",
                                                  " SELECT hypergraph, num_iterations_second, num_plays FROM ",
                                                  DS.tableName DS.hypergraphTableMetadata,
                                                  " CROSS JOIN ",
                                                  temporaryTableName
                                                 ]
  execute insertStatement []
  -- drop the temporary table
  dropTemporaryTableStatement <- prepare connection $ concat ["DROP TABLE ", temporaryTableName]
  execute dropTemporaryTableStatement []
  
appendResults connection experiment@( ED.Stochastic (ED.UCT num_iterations) ED.Perfect num_plays) = do
  let resultTableName = DS.experimentResultTableName experiment
      temporaryTableName = "tmp_" ++ resultTableName
  createTemporaryTableStatement <- prepare connection $ concat ["CREATE TEMPORARY TABLE ", temporaryTableName, " (num_iterations_first INTEGER NOT NULL, num_plays INTEGER NOT NULL)"]
  execute createTemporaryTableStatement []
  insertTempTableStatement <- prepare connection $ concat ["INSERT INTO ", temporaryTableName, " (num_iterations_first, num_plays) VALUES (?, ?)"]
  execute insertTempTableStatement [toSql num_iterations, toSql num_plays]  
  insertStatement <- prepare connection $ concat ["INSERT OR REPLACE INTO ", resultTableName,  
                                                  " (hypergraph, num_iterations_first, num_plays) ",
                                                  " SELECT hypergraph, num_iterations_first, num_plays FROM ",
                                                  DS.tableName DS.hypergraphTableMetadata,
                                                  " CROSS JOIN ",
                                                  temporaryTableName
                                                 ]
  execute insertStatement []
  -- drop the temporary table
  dropTemporaryTableStatement <- prepare connection $ concat ["DROP TABLE ", temporaryTableName]
  execute dropTemporaryTableStatement []
  
appendResults connection experiment@( ED.Stochastic (ED.UCT num_iterations_first) (ED.UCT num_iterations_second) num_plays) = do
  let resultTableName = DS.experimentResultTableName experiment
      temporaryTableName = "tmp_" ++ resultTableName
      
  createTemporaryTableStatement <- prepare connection $ concat ["CREATE TEMPORARY TABLE ", temporaryTableName, " (num_iterations_first INTEGER NOT NULL, num_iterations_second INTEGER NOT NULL, num_plays INTEGER NOT NULL)"]
  execute createTemporaryTableStatement []  
  
  insertTempTableStatement <- prepare connection $ concat ["INSERT INTO ", temporaryTableName, " (num_iterations_first, num_iterations_second, num_plays) VALUES (?, ?, ?)"]
  execute insertTempTableStatement [toSql num_iterations_first, toSql num_iterations_second, toSql num_plays]  
  
  insertStatement <- prepare connection $ concat ["INSERT OR REPLACE INTO ", resultTableName, 
                                                  " (hypergraph, num_iterations_first, num_iterations_second, num_plays) ",                                                  
                                                  " SELECT hypergraph, num_iterations_first, num_iterations_second, num_plays FROM ",
                                                  DS.tableName DS.hypergraphTableMetadata,
                                                  " CROSS JOIN ",
                                                  temporaryTableName
                                                 ]
  execute insertStatement []  
  -- drop the temporary table
  dropTemporaryTableStatement <- prepare connection $ concat ["DROP TABLE ", temporaryTableName]
  execute dropTemporaryTableStatement []
  
  
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
                      errors <- lefts <$> mapM (requireResultTable connection) experiments
                      case errors of
                        [] -> do
                          mapM_ (appendResults connection) experiments
                          commit connection
                        _ -> do
                          putStrLn "the database is errorneously formatted: "
                          mapM_ putStrLn errors
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