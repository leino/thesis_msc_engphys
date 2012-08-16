import GameTheory.PoGa
import System.TimeIt

import Utils.FromHypergraphs (fromFile)


analyseGame game gamesPerTournament fststrat sndstrat = do
  ws <- playTournament gamesPerTournament game fststrat sndstrat
  return $ analyseResults ws

-- play the given perfect games and return the winners
pure games fststrat sndstrat = do
  sequence [playGame (Game game) fststrat sndstrat | game <- games]

probabilistic games numIterations fststrat sndstrat = do
  xss <- sequence [ analyseGame (Game $ Unexplored $ game) numIterations fststrat sndstrat
                  | game <- games ]
  return xss

-- Analyse and print results of a tournament

main :: IO ()
main = do
  let fileName = "../data/hg_4_2_hypergraphs.txt"
  games <- fromFile fileName
  timeIt $ do
    ws <- pure games (return . perfectStrategyFirst) (return . perfectStrategySecond)
    print ws
  
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