module DatabaseStructure
       (TableMetadata (..),
        hypergraphTableMetadata,
        experimentTableMetadata,
        resultTableMetadata)
       where

import Database.HDBC
import ExperimentDescription
import HDBUtils

-- Helpers
stringDesc = SqlColDesc {colType = SqlUnknownT "string", colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing}
integerDesc = SqlColDesc {colType = SqlIntegerT, colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing}

hypergraphTableMetadata = TableMetadata {tableName = "hypergraphs",
                                         createStatement = concat ["CREATE TABLE ", "hypergraphs", 
                                                                   " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                                                   " numvertices INTEGER NOT NULL,",
                                                                   " numedges INTEGER NOT NULL,",
                                                                   " representation STRING NOT NULL)"],
                                         requiredColumns = [("hypergraph", stringDesc),
                                                            ("numvertices", integerDesc),
                                                            ("numedges", integerDesc),
                                                            ("representation", stringDesc)]}

experimentTableMetadata =
  TableMetadata {tableName = "experiments",
                 createStatement = concat ["CREATE TABLE ", "experiments",
                                           " (experiment STRING PRIMARY KEY NOT NULL,",
                                           " strategy_first STRING NOT NULL,",
                                           " strategy_second STRING NOT NULL,",
                                           " num_plays INTEGER)"],
                 requiredColumns = [("experiment", stringDesc),
                                    ("strategy_first", stringDesc),
                                    ("strategy_second", stringDesc),
                                    ("num_plays", integerDesc)]}


experimentResultTableName (Deterministic firstStrategy secondStrategy) =
  "results_" ++ show firstStrategy ++ "_" ++ show secondStrategy
  
experimentResultTableName (Stochastic firstStrategy secondStrategy numPlays) = do
  "results_" ++ show firstStrategy ++ "_" ++ show secondStrategy ++ "_" ++ show numPlays ++ "plays"


-- Result table metadata, given an experiment
resultTableMetadata experiment@(Deterministic _ _) =
  let tableName = experimentResultTableName experiment in
  TableMetadata {tableName = tableName,
                 requiredColumns = [("hypergraph", stringDesc), ("winner", stringDesc)],
                 createStatement = concat ["CREATE TABLE ", tableName,
                                           " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                           " winner STRING)"]}
resultTableMetadata experiment@(Stochastic _ _ _) = 
  let tableName = experimentResultTableName experiment in
  TableMetadata {tableName = tableName,
                 requiredColumns = [("hypergraph", stringDesc),
                                    ("num_first_wins", integerDesc),
                                    ("num_second_wins", integerDesc),
                                    ("num_neither_wins", integerDesc)],
                 createStatement = concat ["CREATE TABLE ", tableName,
                                           " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                           " num_first_wins INTEGER,",
                                           " num_second_wins INTEGER,",
                                           " num_neither_wins INTEGER)"]}

 
