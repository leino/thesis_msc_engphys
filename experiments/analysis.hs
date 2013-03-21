import Data.Functor ((<$>))
import GameTheory.PoGa
import System.Environment (getArgs)
import System.Posix.Env (putEnv)
import Database.HDBC
import Database.HDBC.Sqlite3
import HDBUtils (requireTable)
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
import FromRepresentation (winningSetsFromString)

runAnalysis connection tableName = do
  -- information about the hypergraph (set of set of int)
  let information :: (Set.Set (Set.Set Int)) -> Int
      information h = 
        let edges = h
            edgeSizes = Set.map Set.size edges
            minEdgeSize = minimum $ Set.toList edgeSizes in
        minEdgeSize
        
  -- prepare the insert statement
  insertStatement <- prepare connection $ "INSERT INTO " ++ tableName ++ " VALUES (?,?)"
        
  -- query the database
  let query = "SELECT hypergraph, representation FROM hypergraphs"
  queryResults <- quickQuery connection query []

  let processRow [h, rep] = [h, toSql $ information $ winningSetsFromString $ fromSql rep]
  -- insert into database
  sequence_ [do executeMany insertStatement chunk
                commit connection
            | chunk <- map (map processRow) $ chunksOf 1024 queryResults]

-- Assumes that tableName exists and contains the correct collumns
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName, tableName] -> do
      putEnv "TMPDIR=/usr/tmp" -- put the temporary directory to some place with more storage than /tmp (at least useful on my system, uncomment if you don't have /usr/tmp)
      connection <- connectSqlite3 fileName
      runAnalysis connection tableName
      commit connection
      disconnect connection
    _ -> error "incorrect arguments"
