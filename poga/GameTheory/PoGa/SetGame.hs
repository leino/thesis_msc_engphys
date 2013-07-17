-- A module to use when your game is conveniently
-- definable by sets.

module GameTheory.PoGa.SetGame
       (SetGame(..), makeMove, fromWinningSets)
       where

import Data.Set as Set
import qualified GameTheory.PoGa.Game as G

data SetGame v = SetGame {board :: Set v,               -- all vertices of board
                          turn :: G.Player,               -- who's turn is it?
                          firstChoices :: Set v,        -- vertices that First occupy
                          secondChoices :: Set v,       -- vertices that Second occupy
                          firstWin :: Set v -> Bool,    -- is First winner?
                          secondWin :: Set v -> Bool}   -- is Second winner?
                 
                 
instance Show v => Show (SetGame v) where
  show sg = show $ board sg

-- All unoccupied vertices
availableVertices :: Ord v => SetGame v -> Set v
availableVertices sg =  
  ((board sg) \\ (firstChoices sg)) \\ (secondChoices sg)

-- Current player makes move corresponding to the given vertex
makeMove :: Ord v => SetGame v -> v -> SetGame v
makeMove sg vtx = 
  case turn sg of
    G.First -> sg{firstChoices = Set.insert vtx (firstChoices sg),
                turn = G.Second}
    G.Second -> sg{secondChoices = Set.insert vtx (secondChoices sg),
                 turn = G.First}

-- construct a game given a board and winning sets
fromWinningSets :: (Ord v) =>
  Set.Set v -> Set.Set (Set.Set v) -> Set.Set (Set.Set v) -> SetGame v
fromWinningSets board wssFirst wssSecond = 
  SetGame {board = board,
           turn = G.First,
           firstChoices = Set.empty,
           secondChoices = Set.empty,
           firstWin = win wssFirst,
           secondWin = win wssSecond}
  where
  -- Make a win function from a set of winning sets
  win :: (Ord v) => Set.Set (Set.Set v) -> Set.Set v -> Bool
  win wss s =
    or [ws `Set.isSubsetOf` s | ws <- Set.toList wss]


-- All possible moves that can be made for the given game.
instance Ord v => G.Position (SetGame v) where
  choices sg = 
    Set.toList $ Set.mapMonotonic (makeMove sg) (availableVertices sg)
  winner sg =
    case (firstWin sg $ firstChoices sg,
          secondWin sg $ secondChoices sg) of
      (True, False) -> G.Only G.First
      (False, True) -> G.Only G.Second
      (False, False) -> G.Neither
      (True, True) -> G.Both
  terminal sg =
    (Set.null $ availableVertices sg) || (G.winner sg /= G.Neither)
  turn sg = turn sg