import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getProgName)
import qualified DatabaseStructure as DS
import HDBUtils
import Data.List (intersperse)
import Common
import Data.Functor ((<$>))
import Data.Either (lefts, rights)
import System.Directory (doesFileExist)
  
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
                      putStrLn $ unlines $ map show experiments
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