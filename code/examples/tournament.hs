import GameTheory.PoGa
import Graphics.Graph
import System.Posix.Process (forkProcess)
import Control.Monad.Random
import System.TimeIt

import Utils.FromHypergraphs (fromFile)


analyseGame game gamesPerTournament fststrat sndstrat = do
  ws <- playTournament gamesPerTournament game fststrat sndstrat
  return $ analyseResults ws

-- Analyse and print results of a tournament

main :: IO ()
main = timeIt $ do
  let fststrat = mctsStrategyFirst 400
      sndstrat = return . perfectStrategySecond
  --let game = Game $ Unexplored $ ticTacToe 3
  games <- fromFile "hg_3_3_hypergraphs.txt"
  xss <- sequence [ analyseGame (Game $ Unexplored $ game) 10 fststrat sndstrat | game <- games ]
  print xss
  
-- Analyze the results of a tournament
analyseResults :: [Winner] -> [(Winner, Double)]
analyseResults ws =
  zip outcomes (normalize $ map (tally ws) outcomes)
  where
  outcomes = [Only First, Only Second, Neither]


-- ~~~~~~~ Some handy function for presenting the data ~~~~~~~~~~~~~~~~~~~
  
tally :: Eq a => [a] -> (a -> Int)
tally xs = foldl add z xs
  where
  z = const 0
  add :: Eq a => (a -> Int) -> a -> (a -> Int)
  add t x = \y -> if y == x then (t y) + 1 else t y
  
normalize :: [Int] -> [Double]
normalize ns = 
  let xs = map fromIntegral ns
      n = sum ns in
  map (\x -> x/(fromIntegral n)) xs