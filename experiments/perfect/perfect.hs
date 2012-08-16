import System (getArgs)
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (liftM)

main = do
  arguments <- getArgs
  case arguments of
    [fileName] -> do
      connection <- connectSqlite3 fileName
      (hypergraphs::[String]) <- liftM (concat . (map $ map fromSql)) $ do
        quickQuery' connection "SELECT hypergraphs.hypergraph,representation,numvertices,numedges \
                               \FROM hypergraphs JOIN representations\
                               \ ON hypergraphs.hypergraph=representations.hypergraph \
                               \WHERE numedges = 3 AND numvertices = 3"
                               []
      putStr $ unlines hypergraphs
      disconnect connection
    _ -> error "Error: you must specify exactly one argument: \
               \the filename of the database containing the games (hypergraphs)."