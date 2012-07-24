module GameTheory.PoGa.Games.TicTacToe
       (ticTacToe)
       where

import GameTheory.PoGa.Game
import qualified GameTheory.PoGa.SetGame as SetGame
import qualified Data.Set as Set

board :: Int -> Set.Set (Int, Int)
board n = Set.fromList [(i,j) | i <- [0 .. (n-1)], j <- [0 .. (n-1)]]

winningSets :: Int -> Set.Set (Set.Set (Int, Int))
winningSets n =
  horiz `Set.union` vert `Set.union` diag
  where
  horiz = Set.fromList [Set.fromList [(i,j) | i <- [0 .. n-1]]
                       | j <- [0 .. n-1]]
  vert = Set.fromList [Set.fromList [(i,j) | j <- [0 .. n-1]]
                      | i <- [0 .. n-1]]
  diag = Set.fromList [Set.fromList [(i,j) | i <- [0 .. n-1], j <- [0 .. n-1], i-j==0],
                       Set.fromList [(i,j) | i <- [0 .. n-1], j <- [0 .. n-1], i+j==n-1]]

ticTacToe n = SetGame.fromWinningSets (board n) (winningSets n) (winningSets n)

-- ~~~~~ Properties ~~~~~~~~~~

-- Check that all winning sets are subsets of the board.
prop_saneWinningSets :: Int -> Bool
prop_saneWinningSets n =
  and [ws `Set.isSubsetOf` (board n') | ws <- Set.toList (winningSets n')]
  where
  n' = abs n  
  
-- There should be 2*n+2 "smallest" winning sets in nxn Tic-Tac-Toe
prop_numberOfWinningSets :: Int -> Bool
prop_numberOfWinningSets n = 
  Set.size (winningSets n') == 2*n'+2
  where
  n' = abs n -- only non-negative values
  
-- Check the board size should b n*n = 9
prop_boardSize :: Int -> Bool
prop_boardSize n =
  Set.size (board n') == n'*n'
  where
  n' = abs n  