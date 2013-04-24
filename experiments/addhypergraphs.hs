import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getEnv)
import HypergraphProcessing (batchProcessHypergraphs)
import HDBUtils (requireTable)
import Control.Monad.Instances

-- Arguments: [filename, numVertices, numEdges, nautyArgs]
-- 'filename': the name of the database where the hypergraphs will be inserted
-- 'numVertices': the number of vertices in the desired class of hypergraphs
-- 'numEdges': number of edges in the desired class of hypergraphs
-- 'nautyArgs': additional arguments for Nauty
sanitizeArgs :: [String] -> Either String (String, Int, Int, [String])
sanitizeArgs args = 
  case args of
    fileName:numVertsStr:numEdgesStr:nautyArgs -> do
      let assertPositive varname n = if n >= 0 then Right n else Left $ varname ++ " is not positive"
      numVerts <- assertPositive "number of vertices" $ read numVertsStr
      numEdges <- assertPositive "number of edges" $ read numEdgesStr
      return (fileName, numVerts, numEdges, nautyArgs)
    _ -> Left "wrong number of arguments"

main :: IO ()
main = do
  nautyDir <- getEnv "NAUTYDIR"
  let genbgPath = nautyDir ++ "/genbg"
      showgPath = nautyDir ++ "/showg"  
      chunkSize = 2048 -- Chunksize to use for batches of hypergraphs
  args <- getArgs
  case sanitizeArgs args of
    Left errmsg -> do
      putStrLn $ "error: incorrect arguments: " ++ errmsg
    Right (fileName, numVerts, numEdges, nautyArgs) -> do
      -- Statement for creating the hypergraph table.
      let createStatement =
            "CREATE TABLE hypergraphs (hypergraph STRING PRIMARY KEY NOT NULL, numvertices INTEGER NOT NULL, numedges INTEGER NOT NULL, representation STRING NOT NULL)"
          -- Columns required in order to insert hypergraphs.
          requiredColumns = 
            let stringDesc = SqlColDesc {colType = SqlUnknownT "string", colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing}
                integerDesc = SqlColDesc {colType = SqlIntegerT, colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Nothing} in
            [("hypergraph", stringDesc), ("numvertices", integerDesc), ("numedges", integerDesc), ("representation", stringDesc)]
      -- Create/open database and verify that it has the required columns (or create them if the table doesn't exist)
      maybeConnection <- connectSqlite3 fileName >>= requireTable "hypergraphs" createStatement requiredColumns
      case maybeConnection of
        Nothing -> putStrLn "error: database contains a table named 'hypergraphs', but it is of the wrong format"
        Just connection -> do
          -- This is the main program: given a valid connection, we insert the
          -- described class of hypergraphs in batches, and commit the database after completing each batch.
          -- If there are already entries for the hypergraphs we are inserting, we simply ignore the insert statement
          -- and move on.
          insertStatement <- prepare connection "INSERT OR IGNORE INTO hypergraphs (hypergraph, numvertices, numedges, representation) VALUES (?,?,?,?)"
          batchProcessHypergraphs genbgPath showgPath chunkSize numVerts numEdges nautyArgs $ \g6s rs -> do
            -- process a batch
            executeMany insertStatement [[toSql g6, toSql numVerts, toSql numEdges, toSql r] | (g6, r) <- zip g6s rs]
            commit connection
          disconnect connection