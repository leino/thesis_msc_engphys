module GameTheory.PoGa.Strategy.MCTS
       (
         mctsStrategyFirst,
         mctsStrategySecond,
         unexploredMetaDataNode,
         MCTSNodeData (..)
        )
       where

import qualified GameTheory.PoGa.Game as Game
import qualified Control.Monad.Random as Random
import Test.QuickCheck
import Data.List (find, sortBy, maximumBy, minimumBy)
import Data.Maybe

data MetaDataNode a b p = MetaDataNode {
  firstData :: Maybe a,
  secondData :: Maybe b,
  choices :: [MetaDataNode a b p],
  terminal :: Bool,
  turn :: Game.Player,
  winner :: Game.Winner
  } deriving (Show)

data MCTSNodeData = MCTSNodeData {visitCount :: Int, score :: Score}
newtype MCTSNodeFirst b p = MCTSNodeFirst (MetaDataNode MCTSNodeData b p)
newtype MCTSNodeSecond a p = MCTSNodeSecond (MetaDataNode a MCTSNodeData p)

mctsStrategyFirst :: (Game.Position p, Random.MonadRandom m) =>
                     Int -> MetaDataNode MCTSNodeData b p -> m (MetaDataNode MCTSNodeData b p)
mctsStrategyFirst numIterations node = do
  MCTSNodeFirst node' <- mctsStrategy numIterations (MCTSNodeFirst node)
  return node'
  
mctsStrategySecond :: (Game.Position p, Random.MonadRandom m) =>
                     Int -> MetaDataNode a MCTSNodeData p -> m (MetaDataNode a MCTSNodeData p)
mctsStrategySecond numIterations node = do
  MCTSNodeSecond node' <- mctsStrategy numIterations (MCTSNodeSecond node)
  return node'

-- We can make an empty node with no metadata, given a node
unexploredMetaDataNode :: Game.Position p => p -> MetaDataNode a b p
unexploredMetaDataNode p =
  MetaDataNode {
    firstData = Nothing,
    secondData = Nothing,
    choices = map unexploredMetaDataNode $ Game.choices p,
    turn = Game.turn p,
    terminal = Game.terminal p,
    winner = Game.winner p
    }

class MCTSNode p where
  getMCTSData :: p -> Maybe MCTSNodeData
  setMCTSData :: p -> MCTSNodeData -> p
  setChoices :: p -> [p] -> p
  
explored :: MCTSNode p => p -> Bool
explored = isJust . getMCTSData
  
instance MCTSNode (MCTSNodeFirst b p) where
  getMCTSData (MCTSNodeFirst node) = firstData node
  setMCTSData (MCTSNodeFirst node) md = MCTSNodeFirst $ node {firstData = Just md}
  setChoices (MCTSNodeFirst node) cs =
    let unwrap (MCTSNodeFirst node) = node in
    MCTSNodeFirst $ node {choices = map unwrap cs}
    
instance MCTSNode (MCTSNodeSecond b p) where
  getMCTSData (MCTSNodeSecond node) = secondData node
  setMCTSData (MCTSNodeSecond node) md = MCTSNodeSecond $ node {secondData = Just md}
  setChoices (MCTSNodeSecond node) cs =
    let unwrap (MCTSNodeSecond node) = node in
    MCTSNodeSecond $ node {choices = map unwrap cs}

instance Game.Position p => Game.Position (MetaDataNode a b p) where
  choices mdn = choices mdn
  winner mdn = winner mdn
  terminal mdn = terminal mdn
  turn mdn = turn mdn
  
instance Game.Position p => Game.Position (MCTSNodeFirst b p) where
  choices (MCTSNodeFirst mdn) = map MCTSNodeFirst $ choices mdn
  winner (MCTSNodeFirst mdn) = winner mdn
  terminal (MCTSNodeFirst mdn) = terminal mdn
  turn (MCTSNodeFirst mdn) = turn mdn
  
instance Game.Position p => Game.Position (MCTSNodeSecond a p) where
  choices (MCTSNodeSecond mdn) = map MCTSNodeSecond $ choices mdn
  winner (MCTSNodeSecond mdn) = winner mdn
  terminal (MCTSNodeSecond mdn) = terminal mdn
  turn (MCTSNodeSecond mdn) = turn mdn

type Score = Double

compareChildren :: MCTSNode p => Double -> p -> p -> p -> Ordering
compareChildren cExp node a b =
  case (getMCTSData a, getMCTSData b) of
    (Just aData, Just bData) ->
      let Just parentData = getMCTSData node in
      compare (reconScore parentData aData) (reconScore parentData bData)    
      where
        reconScore :: MCTSNodeData -> MCTSNodeData -> Score
        reconScore parentData childData =
          let (vcp, sp) = (visitCount $ parentData, score $ parentData)
              (vcc, sc) = (visitCount $ childData, score $ childData) in
          ( sc / (fromIntegral vcc) ) +
          cExp * sqrt (  2.0*(log $ fromIntegral vcp) / (fromIntegral vcc)  )
    (Nothing, Nothing) -> EQ
    (Just _, Nothing) -> LT
    (Nothing, Just _) -> GT

-- TODO: rewrite as map best child
-- Also need MCTS nodes to store children in a list
popBestChild :: (MCTSNode p, Game.Position p) => Double -> p -> (p, [p])
popBestChild cExp node
  | explored node = popMaximumBy (compareChildren cExp node) (Game.choices node)
  | otherwise = error "popBestChild: un-explored node given"

findBestMove :: (MCTSNode p, Game.Position p) => p -> p
findBestMove node =
  case filter explored (Game.choices node) of
    [] ->
      error "cannot find a best move: no children are explored"
    explored ->
      maximumBy (compareChildren 0.0 node) explored

mctsStrategy :: (MCTSNode p, Game.Position p, Random.MonadRandom m) => Int -> p -> m p
mctsStrategy 0 node = do
  return $ findBestMove node
mctsStrategy numSteps node = do
  (_, node') <- explore cExp node
  mctsStrategy (numSteps-1) node'
  where
  cExp :: Double  
  cExp = 1.0 / (sqrt 2.0) -- amount of exploration
  
explore :: (MCTSNode p, Game.Position p, Random.MonadRandom m) => Score -> p -> m (Score, p)
explore cExp node = 
  case getMCTSData node of
    Nothing -> do -- not explored
      s <- if Game.terminal node then return $ value node else recon node
      return (s, setMCTSData node $ MCTSNodeData {visitCount = 1, score = s})
    Just (MCTSNodeData {visitCount = vc, score = sc}) -> do
      case Game.terminal node of
        True -> do
          let s = value node
          return (s, setMCTSData node $ MCTSNodeData {visitCount = vc + 1, score = sc + s})            
        False -> do
          let (c, cs) = popBestChild cExp node
          (s, c') <- explore cExp c
          let s' = negate s
              node' = setChoices node (c':cs) in -- score is negated since it is the score of a child node relative to this node
            return (s', setMCTSData node' $ MCTSNodeData {visitCount = vc + 1, score = sc + s'})
  
recon :: (Random.MonadRandom m, MCTSNode p, Game.Position p) => p -> m Score
recon pos
  | Game.terminal pos = return $ value pos
  | otherwise = do
      c <- Random.fromList [(c,1) | c <- Game.choices pos]
      s <- recon c
      return $ -s -- note that score changes sign, since this is the childs score

-- helper function: takes a non-empty list and
-- extracts it's maximum
popMaximumBy :: (a -> a -> Ordering) -> [a] -> (a, [a])
popMaximumBy _ [] = error "popMaximumBy: empty list"
popMaximumBy _ [x] = (x, [])
popMaximumBy cmp (x:xs) = 
  let (m, xs') = popMaximumBy cmp xs in
  if cmp m x == LT then (x, m:xs') else (m, x:xs')

value :: (Game.Position p, MCTSNode p) => p -> Score
value pos
  | Game.turn pos == Game.First = valueFirst pos
  | otherwise = negate $ valueFirst pos

valueFirst :: (Game.Position p, MCTSNode p) => p -> Score
valueFirst node =
  case Game.winner node of
    Game.Neither -> 0.0
    Game.Both -> 0.0
    Game.Only Game.First -> -1.0
    Game.Only Game.Second -> 1.0

-- ~~~~~ Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{-
test_popMaximumBy_preservesLength :: Ord a => a -> [a] -> Bool
test_popMaximumBy_preservesLength x xs =
  let (m, xs') = popMaximumBy compare (x:xs) in
  length xs == length xs'
  
test_popMaximumBy_popsMaximum :: Ord a => a -> [a] -> Bool
test_popMaximumBy_popsMaximum x xs =
  let (m, xs') = popMaximumBy compare (x:xs) in
  m == maximumBy compare (x:xs)

test_compareChildren :: (Arbitrary p, Game.Position p) => Double -> MCTSNode p -> MCTSNode p -> MCTSNode p -> Bool
test_compareChildren cExp parent a@(Unexplored _) b@(Unexplored _) = 
  compareChildren cExp parent a b == EQ
test_compareChildren cExp parent a@(Unexplored _) b@(Explored _ _ _ _) = 
  compareChildren cExp parent a b == LT
test_compareChildren cExp parent a@(Explored _ _ _ _) b@(Unexplored _) = 
  compareChildren cExp parent a b == GT
test_compareChildren cExp parent a@(Explored _ _ _ _) b@(Explored _ _ _ _) = 
  True
-}