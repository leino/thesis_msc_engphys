{-# LANGUAGE FlexibleContexts #-}

module Common
       (readExperiments)
       where 
             
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Functor ((<$>))
import Control.Arrow (left)
import qualified ExperimentDescription as ED
import qualified DatabaseStructure as DS
import Control.Arrow (left)
import Data.Convertible

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
                                                 DS.tableName DS.experimentTableMetadata]) []
