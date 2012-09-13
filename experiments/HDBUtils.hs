module HDBUtils
       (requireTable)
       where

import Database.HDBC
import Data.Functor ((<$>))

requireTable connection tableName createTable requiredColumns = do
  tableExists <- (elem tableName) <$> getTables connection
  case tableExists of
    False -> do
      createTable connection >> commit connection
      return True
    True -> do
      tableValid <- checkForRequiredColumns connection requiredColumns tableName
      case tableValid of
        False -> return False
        True -> return True
  where
  checkForRequiredColumns connection reqCds tableName = do
    cds <- describeTable connection tableName -- collumn decsriptions
    return $ and $ map (flip elem cds) reqCds
