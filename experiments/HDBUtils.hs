module HDBUtils
       (requireTable)
       where

import Database.HDBC
import Data.Functor ((<$>))

requireTable :: IConnection c => String -> String -> [(String, SqlColDesc)] -> c -> IO (Maybe c)
requireTable tableName createStatement requiredColumns connection = do
  tableExists <- (elem tableName) <$> getTables connection
  case tableExists of
    False -> do
      runRaw connection createStatement
      colsOk <- checkForRequiredColumns connection requiredColumns tableName
      case colsOk of
        True -> do
          commit connection
          return $ Just connection
        False -> do
          return Nothing
    True -> do
      tableValid <- checkForRequiredColumns connection requiredColumns tableName
      case tableValid of
        False -> return Nothing
        True -> return $ Just connection
  where
  checkForRequiredColumns connection reqCds tableName = do
    cds <- describeTable connection tableName -- collumn decsriptions
    return $ and $ map (flip elem cds) reqCds
