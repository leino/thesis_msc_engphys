import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Exception.Base (assert)
import System.Environment (getArgs)
import FromRepresentation (winningSetsFromString)
import qualified Data.Set as Set

main = do
  args <- getArgs
  case args of
    [databaseFilename] -> do
      conn <- connectSqlite3 databaseFilename
      results <- quickQuery conn "SELECT hypergraph, representation FROM hypergraphs" []
      insertStatement <- prepare conn "INSERT INTO hypergraph_structure (hypergraph, num_two_sets, num_three_sets) VALUES (?,?,?)"
      inserts <- sequence [do let hypergraph = (fromSql hypergraphSql) :: String
                                  representation = (fromSql representationSql) :: String
                                  wss = ( Set.toList $ winningSetsFromString representation ) :: [Set.Set Int]
                                  ss = (map Set.size wss) :: [Int]
                                  numTwoSets = length $ (filter ((==) 2) ss) :: Int
                                  numThreeSets = length $ (filter ((==) 3) ss) :: Int
                              assert ( (numTwoSets + numThreeSets) == (length ss) ) $ do
                                return [toSql hypergraph, toSql numTwoSets, toSql numThreeSets]
                          | [hypergraphSql, representationSql] <- results]
      executeMany insertStatement inserts
      commit conn
      disconnect conn
    _ -> do
      putStrLn $ "incorrect arguments. please give the filename of the database"