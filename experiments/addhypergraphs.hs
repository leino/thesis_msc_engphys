import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs)
import HypergraphProcessing (batchProcessHypergraphs)
import HDBUtils (requireTable)

-- Arguments: [filename, numVertices, numEdges, nautyArgs]
-- 'filename': the name of the database where the hypergraphs will be inserted
-- 'numVertices': the number of vertices in the desired class of hypergraphs
-- 'numEdges': number of edges in the desired class of hypergraphs
-- 'nautyArgs': additional arguments for Nauty
sanitizeArgs :: [String] -> Maybe (String, Int, Int, [String])
sanitizeArgs args = 
  case args of
    fileName:numVertsStr:numEdgesStr:nautyArgs -> 
      -- TODO: a bit of validation
      Just (fileName, read numVertsStr, read numEdgesStr, nautyArgs)
    _ ->
      Nothing

-- Statement for creating the hypergraph table.
createStatement =
  "CREATE TABLE hypergraphs (hypergraph STRING PRIMARY KEY NOT NULL, numvertices INTEGER NOT NULL, numedges INTEGER NOT NULL, representation STRING NOT NULL)"

-- Columns required in order to insert hypergraphs.
requiredColumns = 
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


-- Connects to a hypergraph database.
connectHypergraphDatabase :: String -> IO (Maybe Connection)
connectHypergraphDatabase fileName = do
  connection <- connectSqlite3 fileName
  -- TODO: refactor: push third argument into the requireTable function except for createStatement
  success <- requireTable connection "hypergraphs" createStatement requiredColumns
  if success then return $ Just connection else return Nothing
    
-- This is the main program: given a valid connection, we insert the
-- described class of hypergraphs in batches, and commit the database after completing each batch.
-- If there are already entries for the hypergraphs we are inserting, we simply ignore the insert statement
-- and move on.
insertHypergraphs numVerts numEdges nautyArgs = \connection -> do
  let chunkSize = 2048
  insertStatement <- prepare connection "INSERT OR IGNORE INTO hypergraphs (hypergraph, numvertices, numedges, representation) VALUES (?,?,?,?)"
  batchProcessHypergraphs chunkSize numVerts numEdges nautyArgs $ \g6s rs -> do
    -- process a batch
    executeMany insertStatement [[toSql g6, toSql numVerts, toSql numEdges, toSql r] | (g6, r) <- zip g6s rs]
    commit connection
  disconnect connection
    
main :: IO ()
main = do
  args <- getArgs
  case sanitizeArgs args of
    Nothing -> do
      putStrLn "error: incorrect arguments"
    Just (fileName, numVerts, numEdges, nautyArgs) -> do
      maybeConnection <- connectHypergraphDatabase fileName
      case maybeConnection of
        Nothing -> putStrLn "error: database contains a table named 'hypergraphs', but it is of the wrong format"
        Just connection -> 
          insertHypergraphs numVerts numEdges nautyArgs connection
          
