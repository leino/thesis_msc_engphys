import Data.Functor ((<$>))
import System.Environment (getArgs)
import System.Posix.Env (putEnv)
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List (sort)
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
import FromRepresentation (winningSetsFromString)

spans :: Eq a => [a] -> [[a]]
spans [] = []
spans xs@(x:_) = let (ys, zs) = span ((==) x) xs in ys:(spans zs)

counts :: Ord a => [a] -> [(a, Int)]
counts = map (\xs@(x:_) -> (x, length xs)) . spans . sort

runAnalysis connection tableName = do
  -- information about the hypergraph (set of set of int)
  let edgeSizes :: (Set.Set (Set.Set Int)) -> [Int]
      edgeSizes = map Set.size . Set.toList
        
  -- prepare the insert statement
  insertStatement <- prepare connection $ "INSERT INTO " ++ tableName ++ " VALUES (?,?,?,?,?)"

  -- query the database
  let query = "SELECT hypergraph, representation FROM hypergraphs"
  queryResults <- quickQuery connection query []

  let processRow [h, rep] =
        let ws = winningSetsFromString $ fromSql $ rep 
            szs = edgeSizes ws
            (numTwoEdges, numThreeEdges) = case counts szs of
              [(2,m), (3,n)] -> (m,n)
              [(2,m)] -> (m,0)
              [(3,n)] -> (0,n)
            minEdge = minimum szs
            maxEdge = maximum szs
        in
        h:(map toSql [minEdge, maxEdge, numTwoEdges, numThreeEdges])
  -- insert into database
  sequence_ [do executeMany insertStatement chunk
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
