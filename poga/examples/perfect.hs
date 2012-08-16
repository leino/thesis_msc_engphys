import GameTheory.PoGa
import System.TimeIt
import System.Environment (getArgs)

import Utils.FromHypergraphs (fromFile)

pure fileName = do
  games <- fromFile fileName
  timeIt $ do
    let fststrat = return . perfectStrategyFirst 
        sndstrat = return . perfectStrategySecond
    ws <- sequence [playGame (Game game) fststrat sndstrat | game <- games]
    print ws

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> pure fileName
    [] -> error "you must give a file name"
    _ -> error "you can give at most one argument"
    

-- ~~~~~ Tools ~~~~~~~~~~~~~~~

tally :: Eq a => [a] -> (a -> Int)
tally xs = foldl add z xs
  where
  z = const 0
  add :: Eq a => (a -> Int) -> a -> (a -> Int)
  add t x = \y -> if y == x then (t y) + 1 else t y