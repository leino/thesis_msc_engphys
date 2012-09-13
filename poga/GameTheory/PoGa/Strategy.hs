module GameTheory.PoGa.Strategy
       (Strategy,
        perfectStrategyFirst,
        perfectStrategySecond,
        randomStrategy,
        mctsStrategyFirst,
        mctsStrategySecond,
        MCTSNode (..))
       where


import Data.Function (on)
import GameTheory.PoGa.Game as Game
import qualified Data.Set as Set
import Data.List (find, sortBy, maximumBy, minimumBy)
import qualified Control.Monad.Random as Random

type Strategy m p = p -> m p

-- ~~~~~~~~~~ Exports ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

perfectStrategyFirst :: (Monad m, Position p) => p -> m p
perfectStrategyFirst pos = return $ perfectStrategy First pos
perfectStrategySecond :: (Monad m, Position p) => p -> m p
perfectStrategySecond pos = return $ perfectStrategy Second pos

-- smaple uniformly from all choices
randomStrategy :: (Random.MonadRandom m, Position p) => p -> m p
randomStrategy pos = Random.fromList [(c,1) | c <- choices pos]


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




-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~ Monte Carlo Tree Search (MCTS) strategy and supporting infrastructure
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Score = Double
data Position p => MCTSNode p = Unexplored p | Explored p Int Score [MCTSNode p]


instance Position p => Position (MCTSNode p) where
  terminal (Unexplored pos) = terminal pos
  terminal (Explored pos _ _ _) = terminal pos
  choices (Unexplored pos) = map Unexplored (choices pos)
  choices (Explored _ _ _ children) = children
  winner (Unexplored pos) = winner pos
  winner (Explored pos _ _ _) = winner pos

compareChildren node a@(Explored _ _ _ _) b@(Explored _ _ _ _) =
  compare (reconScore node a) (reconScore node b)
  where
  cExp :: Double  
  cExp = 1.0 / (sqrt 2.0) -- amount of exploration
  reconScore :: (Position p) => MCTSNode p -> MCTSNode p -> Score
  reconScore parent@(Explored _ vcp sp _) child@(Explored _ vcc sc _) =
    ( sc / (fromIntegral vcc) ) +
    cExp * sqrt (  2.0*(log $ fromIntegral vcp) / (fromIntegral vcc)  )
compareChildren _ (Unexplored _) (Unexplored _) = EQ
compareChildren _ (Explored _ _ _ _) (Unexplored _) = LT
compareChildren _ (Unexplored _) (Explored _ _ _ _) = GT 


-- This function is only defined for non-terminal nodes
popBestChild :: Position p => MCTSNode p -> (MCTSNode p, [MCTSNode p])
popBestChild node@(Explored pos visitCount score children) = 
  popMaximumBy (compareChildren node) children
popBestChild (Unexplored _) = error "popBestChild: un-explored node given"



mctsStrategy :: (Position p, Random.MonadRandom m) => (p -> Score) -> Int -> MCTSNode p -> m (MCTSNode p)
mctsStrategy leafValue 0 node = do
  let (c, _) = popBestChild node
  return c
mctsStrategy leafValue numSteps node = do
  (_, node') <- explore leafValue node
  mctsStrategy leafValue (numSteps-1) node'



explore :: (Position p, Random.MonadRandom m) => (p -> Score) -> MCTSNode p -> m (Score, MCTSNode p)
explore leafValue (Unexplored pos) = do
  s <- recon leafValue pos
  return $ (-s, Explored pos 1 s (map Unexplored $ choices pos)) --note that the score changes sign
explore leafValue node@(Explored pos visitCount score children)
  | terminal node = do
      let s = leafValue pos
      return $ (-s, Explored pos (visitCount+1) s []) -- note that score changes sign
  | otherwise = do
      let (c, cs) = popBestChild node
      (s, c') <- explore leafValue c
      return $ (-s, Explored pos (visitCount+1) (score+s) (c':cs)) -- note that score changes sign    

recon :: (Random.MonadRandom m, Position p) => (p -> Score) -> p -> m Score
recon leafValue pos
  | terminal pos = return $ leafValue pos
  | otherwise = do
      c <- Random.fromList [(c,1) | c <- choices pos]
      s <- recon leafValue c
      return $ -s

-- helper function: takes a non-empty list and
-- extracts it's maximum
popMaximumBy :: (a -> a -> Ordering) -> [a] -> (a, [a])
popMaximumBy _ [] = error "popMaximumBy: empty list"
popMaximumBy _ [x] = (x, [])
popMaximumBy cmp (x:xs) = 
  let (m, xs') = popMaximumBy cmp xs in
  if cmp m x == LT then (x, m:xs') else (m, x:xs')





valueFirst :: Position p => p -> Score
valueFirst pos =
  case winner pos of
    Neither -> 0.0
    Both -> 0.0
    Only First -> -1.0
    Only Second -> 1.0

valueSecond :: Position p =>  p -> Score
valueSecond = negate . valueFirst


mctsStrategyFirst :: (Position p, Random.MonadRandom m) => Int -> MCTSNode p ->  m (MCTSNode p) 
mctsStrategyFirst = mctsStrategy valueFirst
mctsStrategySecond :: (Position p, Random.MonadRandom m) => Int -> MCTSNode p ->  m (MCTSNode p)
mctsStrategySecond = mctsStrategy valueSecond




-- ~~~~~ Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~



test_popMaximumBy :: Ord a => a -> [a] -> Bool
test_popMaximumBy x xs =
  let (m, xs') = popMaximumBy compare (x:xs) in
  length (x:xs) == length (m:xs') && m == maximumBy compare (x:xs)