import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs, getProgName, getEnvironment)
import HypergraphProcessing (batchProcessHypergraphs)
import HDBUtils (requireTable)
import Control.Monad.Instances
import Data.Functor ((<$>))
import Data.List (lookup)

-- Arguments: [filename, numVertices, numEdges, nautyArgs]
-- 'filename': the name of the database where the hypergraphs will be inserted
-- 'numVertices': the number of vertices in the desired class of hypergraphs
-- 'numEdges': number of edges in the desired class of hypergraphs
-- 'nautyArgs': additional arguments for Nauty
sanitizeArgs :: [String] -> Either String (String, Int, Int, [String])
sanitizeArgs args = 
  case args of
    fileName:numVertsStr:numEdgesStr:nautyArgs -> do
      -- some helpers
      let assertInteger :: String -> String -> Either String Int
          assertInteger varname str = 
            case reads str of
              [(n, "")] -> Right n
              _ -> Left $ varname ++ " is not an integer"
          assertPositive :: String -> Int -> Either String Int
          assertPositive varname n = if n >= 0 then Right n else Left $ varname ++ " is not positive"
      -- use the helpers to read the arguments and give appropriate errors
      numVerts <- assertInteger "second argument" numVertsStr >>= assertPositive "number of vertices"
      numEdges <- assertInteger "third argument" numEdgesStr >>= assertPositive "number of edges"
      return (fileName, numVerts, numEdges, nautyArgs)
    _ -> Left "wrong number of arguments"

usage programName = unlines [programName ++ " [filename] [number of vertices] [number of edges] [Optionally: extra arguments for genbg]",
                             "filename: the file name of the database you wish to add the hypergraphs to. If the file does not exist then it is created.",
                             "number of vertices: the number of vertices of the class of hypergraphs you wish to add",
                             "number of edges: the number of edges of the class of hypergraphs you wish to add",
                             "extra arguments for genbg: any extra arguments you need to pass to Nauty's genbg tool in order to describe the hypergraph class. The -z argument is always added.",
                             "  see 'genbg --help for a complete list of options'"]

main :: IO ()
main = do
  maybeNautyDir <- lookup "NAUTYDIR" <$> getEnvironment
  case maybeNautyDir of
    Nothing -> do
      putStrLn "error: environment variable 'NAUTYDIR' is not set. Please set using 'export NAUTYDIR=/path/to/nauty'. The given path should countain Nauty's 'genbg' and 'showg' tools."
    Just nautyDir -> do
      let genbgPath = nautyDir ++ "/genbg"
          showgPath = nautyDir ++ "/showg"  
          chunkSize = 2048 -- Chunksize to use for batches of hypergraphs
      args <- getArgs
      programName <- getProgName        
      case sanitizeArgs args of
        Left errmsg -> do
          putStrLn $ "error: incorrect arguments: " ++ errmsg
          putStrLn $ "usage: \n" ++ usage programName
        Right (fileName, numVerts, numEdges, nautyArgs) -> do
          
          let createStatement = "CREATE TABLE hypergraphs (hypergraph STRING PRIMARY KEY NOT NULL, numvertices INTEGER NOT NULL, numedges INTEGER NOT NULL, representation STRING NOT NULL)"
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