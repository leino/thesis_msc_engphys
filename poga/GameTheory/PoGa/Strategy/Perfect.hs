module GameTheory.PoGa.Strategy.Perfect where

import GameTheory.PoGa.Game
import Data.List (find, sortBy, maximumBy, minimumBy)
import Data.Function (on)

perfectStrategyFirst :: (Monad m, Position p) => p -> m p
perfectStrategyFirst pos = return $ perfectStrategy First pos
perfectStrategySecond :: (Monad m, Position p) => p -> m p
perfectStrategySecond pos = return $ perfectStrategy Second pos

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prunedMaximumBy :: (a -> a -> Ordering) -> (a -> Bool) -> [a] -> a
prunedMaximumBy compare isMax xs = 
  case find isMax xs of
    Just x -> x
    Nothing -> maximumBy compare xs

alternatingStrategy :: Position p => (Winner -> Winner -> Ordering) -> Player -> p -> Winner
alternatingStrategy compareWinners player pos
  | terminal pos = winner pos
  | otherwise =
      let ws = map (alternatingStrategy (flip compareWinners) (opponent player)) (choices pos) in
      prunedMaximumBy compareWinners ((==) (Only player)) ws
    
-- first is 'maxi' since Only First is greatest, in compareWinners
perfectStrategy:: Position p => Player -> p -> p
perfectStrategy player pos = 
  let cs = choices pos
      ws = map (alternatingStrategy (flip compareWinners) (opponent player)) cs
      wcs = zip ws cs in
  snd $ prunedMaximumBy (compareWinners `on` fst) ((==) (Only player) . fst) wcs
  where
    winnerValue w | w == (Only player) = 1 | w == (Only $ opponent player) = -1 | otherwise = 0
    compareWinners w w' = compare (winnerValue w) (winnerValue w') 