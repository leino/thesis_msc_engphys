module HDBUtils
       (requireTable)
       where

import Database.HDBC
import Data.Functor ((<$>))

requireTable :: IConnection c => c -> String -> String -> [(String, SqlColDesc)] -> IO Bool
requireTable connection tableName createStatement requiredColumns = do
  tableExists <- (elem tableName) <$> getTables connection
  case tableExists of
    False -> do
      runRaw connection createStatement
      colsOk <- checkForRequiredColumns connection requiredColumns tableName
      case colsOk of
        True -> do
          commit connection
          return True
        False -> do
          return False
    True -> do
      tableValid <- checkForRequiredColumns connection requiredColumns tableName
      case tableValid of
        False -> return False
        True -> return True
  where
  checkForRequiredColumns connection reqCds tableName = do
    cds <- describeTable connection tableName -- collumn decsriptions
    return $ and $ map (flip elem cds) reqCds
