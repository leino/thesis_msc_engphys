{-# LANGUAGE FlexibleContexts #-}

module HDBUtils
       (TableMetadata (..),
        TablePresence (..),
        requireTable,
        checkTablePresence,
        safeSqlConvert)
       where

import Database.HDBC
import Data.Functor ((<$>))
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Control.Arrow (left)
import Data.Convertible


safeSqlConvert :: Convertible SqlValue a => SqlValue -> Either String a
safeSqlConvert = left prettyConvertError . safeFromSql

data TableMetadata = TableMetadata {tableName :: String,
                                    createStatement :: String,
                                    requiredColumns :: [(String, SqlColDesc)]}

data TablePresence = Present | Invalid | Missing

checkTablePresence :: IConnection c => TableMetadata -> c -> IO TablePresence
checkTablePresence (TableMetadata tableName _ requiredColumns) connection = do
  exists <- (elem tableName) <$> getTables connection
  case exists of
    False -> return Missing
    True -> do
      valid <- hasRequiredColumns requiredColumns tableName connection
      case valid of
        True -> return Present
        False -> return Invalid

  where
    hasRequiredColumns reqCds tableName connection = do
      cds <- describeTable connection tableName -- collumn decsriptions
      return $ and $ map (flip elem cds) reqCds
    
requireTable :: IConnection c => TableMetadata -> c -> IO (Either String c)
requireTable tmd@(TableMetadata tableName createStatement requiredColumns) connection = do
  presence <- checkTablePresence tmd connection
  case presence of
    Missing -> do
      runRaw connection createStatement
      newPresence <- checkTablePresence tmd connection
      case newPresence of
        Present -> do
          commit connection
          return $ Right connection
        _ -> do
          return $ Left "the required table is missing and could not be created successfully"
    Invalid -> return $ Left "a table with the same name already existed, but it had invalid structure"
    Present -> return $ Right connection
