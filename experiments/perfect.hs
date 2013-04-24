import Data.Functor ((<$>))
import GameTheory.PoGa
import System (getArgs)
import Database.HDBC
import Database.HDBC.Sqlite3
import HDBUtils (requireTable)
import Data.List.Split (chunksOf)
import FromRepresentation (fromRepresentation)

requiredColumns = 
  [("hypergraph", SqlColDesc {colType = SqlUnknownT "string",
                              colSize = Nothing,
                              colOctetLength = Nothing,
                              colDecDigits = Nothing,
                              colNullable = Nothing}),
   ("winner", SqlColDesc {colType = SqlUnknownT "string",
                          colSize = Nothing,
                          colOctetLength = Nothing,
                          colDecDigits = Nothing,
                          colNullable = Nothing})]

runExperiment connection tableName = do

  let showWinner :: Winner -> String  
      showWinner (Only First) = "First"
      showWinner (Only Second) = "Second"                      
      showWinner Neither = "Neither"
  
  let result game = do
        r <- showWinner <$> playGame (Game game) perfectStrategyFirst perfectStrategySecond
        return r

  -- Get all the rows that need a result computed
  let query = concat ["SELECT ",
                      "hypergraph, numvertices, representation",
                      " FROM ",
                      tableName, " NATURAL JOIN hypergraphs",
                      " WHERE winner IS NULL"
                     ]

  -- Insert results
  insertStatement <- do prepare connection $ concat ["REPLACE INTO ", tableName, 
                                                     " (hypergraph, winner) VALUES (?,?)"]
  results <- map convertResult <$> quickQuery connection query []

  -- Compute the results in chunks
  let rcs = chunksOf 1024 results

  sequence_ [do putStr $ "processing chunk " ++ show i ++ "... "
                rs <- sequence [do r <- result sg
                                   return $ [toSql (h::String), toSql (r::String)]
                               | (h, sg) <- rc]
                executeMany insertStatement rs
                commit connection
                putStrLn $ "done!"
            |(rc,i) <- zip rcs [1 ..]]
  where      
    convertResult [h, numvertices, rep] = (fromSql h, fromRepresentation (fromSql numvertices) (fromSql rep)) :: (String, SetGame Int)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName, experimentName] -> do
      connection <- connectSqlite3 fileName
      -- ensure that the required table exists
      let createTable connection = do runRaw connection $ concat ["CREATE TABLE ", experimentName, " ", 
                                                                  "(hypergraph STRING NOT NULL, \
                                                                  \winner STRING NOT NULL)"]    
      requireTable connection experimentName createTable requiredColumns
      runExperiment connection experimentName
      commit connection
      disconnect connection
    _ -> error "incorrect arguments"
