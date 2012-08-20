import System (getArgs)
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Functor ((<$>))
import Control.Monad (when)
import GameTheory.PoGa
import FromRepresentation (fromRepresentation)

main = do
  arguments <- getArgs
  case arguments of
    [fileName, experimentName] -> do
      connection <- connectSqlite3 fileName
      -- make sure that we have the column which will hold the results of our computations
      validTable <- elem "winner" <$> map fst <$> describeTable connection experimentName
      when (not validTable) $ do
        runRaw connection $ "ALTER TABLE " ++ experimentName ++ " ADD winner STRING"
        commit connection
      -- query for all pairs of hypergraphs and representations
      let query = concat ["SELECT hypergraphs.hypergraph, \
                          \hypergraphs.numvertices, \
                          \hypergraphs.numedges, \
                          \representations.representation \
                          \FROM ",
                          experimentName, " NATURAL JOIN \
                          \representations NATURAL JOIN \
                          \hypergraphs"]
      results <- map convertResult <$> quickQuery' connection query []
      -- do the computation
      hws <- sequence [do w <- playGame (Game sg) perfectStrategyFirst perfectStrategySecond
                          return (h, w)
                      |(h, nv, ne, sg) <- results]
      -- insert results of computation
      insertStatement <- prepare connection $ "REPLACE INTO " ++ experimentName ++ " (hypergraph, winner) VALUES (?,?)"
      executeMany insertStatement [[toSql h, toSql $ showWinner w] | (h,w) <- hws]
      commit connection
      disconnect connection
      where
      convertResult [h, nv, ne, r] = 
        (fromSql h, fromSql nv, fromSql ne, fromRepresentation $ fromSql r) :: (String, Int, Int, SetGame Int)
      showWinner (Only First) = "First"
      showWinner (Only Second) = "Second"                      
      showWinner Neither = "Neither"
    _ -> error "incorrect arguments"