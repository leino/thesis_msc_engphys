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
                                           " (name STRING PRIMARY KEY NOT NULL,",
                                           " strategy_first STRING NOT NULL,",
                                           " strategy_second STRING NOT NULL,",
                                           " num_plays INTEGER)"],
                 requiredColumns = [("name", stringDesc),
                                    ("strategy_first", stringDesc),
                                    ("strategy_second", stringDesc),
                                    ("num_plays", integerDesc)]}

experimentResultTableName :: Experiment -> String
experimentResultTableName experiment = 
  concat ["results_",
          showKind $ firstStrategy experiment,
          "_vs_",
          showKind $ secondStrategy experiment]

-- Result table metadata, given an experiment
resultTableMetadata experiment@(Deterministic _ _) =
  let tableName = experimentResultTableName experiment in
  TableMetadata {tableName = tableName,
                 requiredColumns = [("hypergraph", stringDesc), ("winner", stringDesc)],
                 createStatement = concat ["CREATE TABLE ", tableName,
                                           " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                           " winner STRING)"]}
  
resultTableMetadata experiment@(Stochastic (UCT _) (UCT _) _) = 
  let tableName = experimentResultTableName experiment in
  TableMetadata {tableName = tableName,
                 requiredColumns = [("hypergraph", stringDesc),
                                    ("num_iterations_first", integerDesc),
                                    ("num_iterations_second", integerDesc),                                    
                                    ("num_first_wins", integerDesc),
                                    ("num_second_wins", integerDesc),
                                    ("num_neither_wins", integerDesc),
                                    ("num_plays", integerDesc)
                                   ],
                 createStatement = concat ["CREATE TABLE ", tableName,
                                           " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                           " num_iterations_first INTEGER NOT NULL,",
                                           " num_iterations_second INTEGER NOT NULL,",
                                           " num_first_wins INTEGER,",
                                           " num_second_wins INTEGER,",
                                           " num_neither_wins INTEGER,",
                                           " num_plays INTEGER NOT NULL)"
                                          ]}
 
resultTableMetadata experiment@(Stochastic Perfect (UCT _) _) = 
  let tableName = experimentResultTableName experiment in
  TableMetadata {tableName = tableName,
                 requiredColumns = [("hypergraph", stringDesc),
                                    ("num_iterations_second", integerDesc),                                    
                                    ("num_first_wins", integerDesc),
                                    ("num_second_wins", integerDesc),
                                    ("num_neither_wins", integerDesc),
                                    ("num_plays", integerDesc)
                                    ],
                 createStatement = concat ["CREATE TABLE ", tableName,
                                           " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                           " num_iterations_second INTEGER NOT NULL,",
                                           " num_first_wins INTEGER,",
                                           " num_second_wins INTEGER,",
                                           " num_neither_wins INTEGER,",
                                           " num_plays INTEGER NOT NULL)"
                                           ]}
 
resultTableMetadata experiment@(Stochastic (UCT _) Perfect _) = 
  let tableName = experimentResultTableName experiment in
  TableMetadata {tableName = tableName,
                 requiredColumns = [("hypergraph", stringDesc),
                                    ("num_iterations_first", integerDesc),
                                    ("num_first_wins", integerDesc),
                                    ("num_second_wins", integerDesc),
                                    ("num_neither_wins", integerDesc),
                                    ("num_plays", integerDesc)                                    
                                   ],
                 createStatement = concat ["CREATE TABLE ", tableName,
                                           " (hypergraph STRING PRIMARY KEY NOT NULL,",
                                           " num_iterations_first INTEGER NOT NULL,",
                                           " num_first_wins INTEGER,",
                                           " num_second_wins INTEGER,",
                                           " num_neither_wins INTEGER,",
                                           " num_plays INTEGER NOT NULL)"
                                           ]}
 
