import Data.Functor ((<$>))
import GameTheory.PoGa
import System.Environment (getArgs)
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List.Split (chunksOf)
import FromRepresentation (fromRepresentation)
import HDBUtils (requireTable)
import Control.Monad (unless)


-- Columns added for random experiments:
--
-- Metadata about the experiment:
-- hypergraph      :  g6 format hypergraph which represents the game
-- numIterations   :  number of iterations for the randomized strategy (mcts)
--
-- Experiment results:
-- numFirstWins    :  number of times first has won
-- numSecondWins   :  number of times second has won
-- numNeitherWins  :  number of times neither has won


requiredColumns = 
  [("hypergraph", SqlColDesc {colType = SqlUnknownT "string",
                              colSize = Nothing,
                              colOctetLength = Nothing,
                              colDecDigits = Nothing,
                              colNullable = Nothing}),
   ("numiterations", SqlColDesc {colType = SqlIntegerT,
                                 colSize = Nothing,
                                 colOctetLength = Nothing,
                                 colDecDigits = Nothing,
                                 colNullable = Nothing}),
   ("numfirstwins", SqlColDesc {colType = SqlIntegerT,
                                colSize = Nothing,
                                colOctetLength = Nothing,
                                colDecDigits = Nothing,
                                colNullable = Nothing}),
   ("numsecondwins", SqlColDesc {colType = SqlIntegerT,
                                 colSize = Nothing,
                                 colOctetLength = Nothing,
                                 colDecDigits = Nothing,
                                 colNullable = Nothing}),
   ("numneitherwins", SqlColDesc {colType = SqlIntegerT,
                                  colSize = Nothing,
                                  colOctetLength = Nothing,
                                  colDecDigits = Nothing,
                                  colNullable = Nothing})]


runExperiment connection tableName numGames = do
  let showWinner :: Winner -> String  
      showWinner (Only First) = "First"
      showWinner (Only Second) = "Second"                      
      showWinner Neither = "Neither"
      
  let result numIterations game = convertResults <$> playTournament numGames
                                                                    (Game $ Unexplored game) 
                                                                    (mctsStrategyFirst numIterations)
                                                                    perfectStrategySecond

  -- Get all the rows that need a result computed
  let query = concat ["SELECT ",
                      "hypergraph, numiterations, numvertices, representation",
                      " FROM ",
                      tableName, " NATURAL JOIN hypergraphs",
                      " WHERE numfirstwins IS NULL OR numsecondwins IS NULL OR numneitherwins IS NULL"
                     ]

  -- Insert results
  insertStatement <- do prepare connection $ concat ["REPLACE INTO ", tableName, 
                                                     " (hypergraph, numiterations, numfirstwins, numsecondwins, numneitherwins) VALUES (?,?,?,?,?)"]

  results <- map convertResult <$> quickQuery connection query []

  -- Compute the results in chunks
  let rcs = chunksOf 1024  results

  sequence_ [do putStr $ "processing chunk " ++ show i ++ "... "
                rs <- sequence [do r <- result numIterations sg
                                   return $ ( toSql (h::String) ) : (toSql (numIterations :: Int)) : ( map toSql r )
                               | (h, numIterations, sg) <- rc]
                executeMany insertStatement rs
                commit connection
                putStrLn $ "done!"
            |(rc,i) <- zip rcs [1 ..]]
  where
  convertResult [h, numiterations, numvertices, rep] = (fromSql h, fromSql numiterations, fromRepresentation (fromSql numvertices) (fromSql rep)) :: (String, Int, SetGame Int)
  convertResults :: [Winner] -> [Int]
  convertResults ws =
    let (nf, ns, nn) = tallyResults ws in [nf, ns, nn]
  tallyResults :: [Winner] -> (Int, Int, Int)
  tallyResults [] = (0,0,0)
  tallyResults (w:ws) = 
    let (nf, ns, nn) = tallyResults ws in
    case w of
      Only First -> (nf+1, ns, nn)
      Only Second -> (nf, ns+1, nn)
      Neither -> (nf, ns, nn+1)

main :: IO ()
main = do
  [fileName, experimentName, numGamesStr] <- getArgs
  let numGames = read numGamesStr :: Int

  let createTable connection = do runRaw connection $ concat ["CREATE TABLE ", experimentName, " ", 
                                                              "(hypergraph STRING NOT NULL, \
                                                               \numiterations INTEGER NOT NULL, \
                                                               \numfirstwins INTEGER, \
                                                               \numsecondwins INTEGER, \
                                                               \numneitherwins INTEGER)"]
  
  connection <- connectSqlite3 fileName
  success <- requireTable connection experimentName createTable requiredColumns
  unless success $ do error "something is wrong with the database"
  runExperiment connection experimentName numGames
  disconnect connection
    
