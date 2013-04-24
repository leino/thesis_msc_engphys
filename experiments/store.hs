import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when, unless)
import Data.Functor ((<$>))
import System.Environment (getArgs)
import HypergraphProcessing
import HDBUtils

main = do
  args <- getArgs
  case args of
    databaseFileName:experimentName:numVertsStr:numEdgesStr:nautyArgs -> do
      let (numVerts,numEdges) = (read numVertsStr, read numEdgesStr) :: (Int, Int)
      storeResults databaseFileName experimentName numVerts numEdges nautyArgs
    _ -> putStrLn "incorrect arguments"
  
storeResults databaseFileName experimentName numVerts numEdges nautyArgs = do
  let createStatement =   "CREATE TABLE hypergraphs (hypergraph STRING PRIMARY KEY NOT NULL,\
                          \numvertices INTEGER NOT NULL,\
                          \numedges INTEGER NOT NULL,\
                          \representation STRING NOT NULL)"

  connection <- connectSqlite3 databaseFileName
  -- set up data base
  acquiredHyp <- requireTable connection
                              "hypergraphs"
                              createStatement
                              requiredHypergraphColumns
  acquiredExp <- requireTable connection
                              experimentName
                              (createExperimentStatement experimentName)
                              requiredExperimentColumns
  unless (acquiredHyp && acquiredExp) $ error "database is invalid"
  -- this batch processor inserts a batch into the hypergraphs table
  let representBatch g6s rs = do
        insertStatement <- prepare connection "INSERT OR IGNORE INTO hypergraphs \
                                              \(hypergraph, numvertices, numedges, representation) \
                                              \VALUES (?,?,?,?)"
        executeMany insertStatement [[toSql g6, toSql numVerts, toSql numEdges, toSql r]
                                    |(g6, r) <- zip g6s rs]
        commit connection
  -- this batch processor inserts the hypergraphs into the experiment table
  let planBatch g6s = do
        insertStatement <- prepare connection $ concat ["INSERT OR IGNORE INTO ",
                                                        experimentName,
                                                        " (hypergraph) VALUES (?)"]
        executeMany insertStatement [[toSql g6] | g6 <- g6s]
        commit connection
  -- now we do both on all of the hypergraphs defined by numVerts, numEdges and nautyArgs
  batchProcessHypergraphs 2048 numVerts numEdges nautyArgs (\g6s rs -> representBatch g6s rs >> planBatch g6s)
  disconnect connection

createExperimentStatement experimentName =
  concat ["CREATE TABLE ",
          experimentName,
          " (hypergraph STRING PRIMARY KEY NOT NULL REFERENCES hypergraphs(hypergraph))"]

requiredHypergraphColumns = 
  [("hypergraph", SqlColDesc {colType = SqlUnknownT "string",
                              colSize = Nothing,
                              colOctetLength = Nothing,
                              colDecDigits = Nothing,
                              colNullable = Nothing}),
   ("numvertices", SqlColDesc {colType = SqlIntegerT,
                               colSize = Nothing,
                               colOctetLength = Nothing,
                               colDecDigits = Nothing,
                               colNullable = Nothing}),
   ("numedges", SqlColDesc {colType = SqlIntegerT,
                            colSize = Nothing,
                            colOctetLength = Nothing,
                            colDecDigits = Nothing,
                            colNullable = Nothing}),
   ("representation", SqlColDesc {colType = SqlUnknownT "string",
                                  colSize = Nothing,
                                  colOctetLength = Nothing,
                                  colDecDigits = Nothing,
                                  colNullable = Nothing})]


requiredExperimentColumns =
  [("hypergraph", SqlColDesc {colType = SqlUnknownT "string",
                              colSize = Nothing,
                              colOctetLength = Nothing,
                              colDecDigits = Nothing,
                              colNullable = Nothing})]