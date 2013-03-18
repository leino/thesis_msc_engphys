import Data.Functor ((<$>))
import GameTheory.PoGa
import System (getArgs)
import Database.HDBC
import Database.HDBC.Sqlite3
import HDBUtils (requireTable)
import Data.List.Split (chunksOf)
import FromRepresentation (winningSetsFromString)

runAnalysis connection tableName = do
  -- information about the hypergraph (set of set of int)
  let information h = 
        let edges = h
            numEdges = Set.size edges
            edgeSizes = Set.map Set.size edges
            minEdgeSize = minimum edgeSizes in
        (minEdgeSize, numEdges)
                                   
  -- query the database
  let query = concat ["SELECT ", "hypergraph, representation"]
  queryResults <- map ( \[h, rep] -> (fromSql h, winningSetsFromString$fromSql$rep) ) <$> quickQuery connection query []
  -- queryResults :: [(String, Set.Set (Set.Set Int))] 

  -- process query results in chunks
  sequence_ [do putStr $ "processing chunk " ++ show i ++ "... "
                rs <- sequence [do (numEdgeSize, numEdges) <- information sg
                                   return $ [toSql (h::String), toSql (r::String)]
                               | (h, sg) <- chunk]
            | (chunk, i) <- zip (chunksOf 100 queryResults) [1 ..]]

-- Assumes that tableName exists and contains the correct collumns
main :: IO ()
main = do
  let experimentName = "analysis"
  args <- getArgs
  case args of
    [fileName, tableName] -> do
      putEnv "TMPDIR=/usr/tmp" -- put the temporary directory to some place with more storage than /tmp (at least useful on my system, uncomment if you don't have /usr/tmp)
      connection <- connectSqlite3 fileName
      runAnalysis connection experimentName
      disconnect connection
    _ -> error "incorrect arguments"
