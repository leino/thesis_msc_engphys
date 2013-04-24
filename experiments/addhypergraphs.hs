import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (unless)
import System.Environment (getArgs)
import HypergraphProcessing
import HDBUtils

-- statement for creating the hypergraph table
createStatement =
  "CREATE TABLE hypergraphs (hypergraph STRING PRIMARY KEY NOT NULL, numvertices INTEGER NOT NULL, numedges INTEGER NOT NULL, representation STRING NOT NULL)"

-- connects to a hypergraph database
connectHypergraphDatabase :: String -> IO (Maybe SQLITEConnection)
connectHypergraphDatabase fileName = do
  connection <- connectSqlite3 fileName
  success <- requireTable connection "hypergraphs" (runRaw connection createStatement) requiredHypergraphColumns -- TODO: refactor: push third argument into the requireTable function except for createStatement
  if success then return $ Just connection else return Nothing
  
getArguments :: [String] -> Maybe (String, Int, Int, String)
getArguments args = 
  case args of
    fileName:numVertsStr:numEdgesStr:nautyArgs -> 
      -- TODO: a bit of validation
      Just (fileName, read numVertsStr, read numEdgesStr, nautyArgs)

main = do
  getArgs >>= getArguments >>= 
    maybe (putStrLn "incorrect arguments")
          (\(fileName, numVerts, numEdges, nautyArgs) -> do
              connectHypergraphDatabase fileName >>=
                maybe (putStrLn "database contains a table named hypergraphs, but it is of the wrong format")
                      (do insertHypergraphs experimentName numVerts numEdges nautyArgs connection
                          disconnect connection) )
  
insertHypergraphs numVerts numEdges nautyArgs connection = do
  -- set up data base
  let statement = "INSERT OR IGNORE INTO hypergraphs (hypergraph, numvertices, numedges, representation) VALUES (?,?,?,?)"
  batchProcessHypergraphs 2048 numVerts numEdges nautyArgs $ \g6s rs -> do
    -- process a batch
    insertStatement <- prepare connection statement
    executeMany insertStatement [[toSql g6, toSql numVerts, toSql numEdges, toSql r] |(g6, r) <- zip g6s rs]
    commit connection