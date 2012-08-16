module GameTheory.PoGa.Games.Hex
       (hex)
       where

import Test.QuickCheck
import Data.Array
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import qualified Data.Set as Set
import GameTheory.PoGa.Game
import qualified GameTheory.PoGa.SetGame as SetGame


-- ~~~~~ Exports ~~~~~~~~~~

hex :: Int -> Int -> SetGame.SetGame (Int, Int)
hex nrows ncols = SetGame.SetGame {SetGame.board = board nrows ncols,
                                   SetGame.turn = First,
                                   SetGame.firstWin = firstWin nrows ncols, 
                                   SetGame.secondWin = secondWin nrows ncols,
                                   SetGame.firstChoices = Set.empty,
                                   SetGame.secondChoices = Set.empty}


-----------------------------------------


board :: Int -> Int -> Set.Set (Int, Int)
board nrows ncols = Set.fromList [(i,j) | i <- [0 .. nrows-1], j <- [0 .. ncols-1]]


-- A winning set in hex
firstWin :: Int -> Int -> Set.Set (Int, Int) -> Bool
firstWin = win

secondWin :: Int -> Int -> Set.Set (Int, Int) -> Bool 
secondWin = win



-- ~~~~~ Stuff to define the win function ~~~~~~~~~~



-- neighbours (i,j) are the hexagonal neighbours of
-- the (i,j)-cell, as specified in the scheme above.
-- Note that a given cell has 6 neighbours.
-- Also, I have set it up so that there is no 
-- "above" or "below" neighbours. This makes sense if
-- you look at a picture of a hexagonal net.
neighbours :: (Int, Int) -> Set.Set (Int, Int)
neighbours (i,j) = 
  Set.fromList [(i+1,j-1), -- above-left
                (i+1,j),   -- above-right
                (i-1,j),   -- below-left
                (i-1,j+1), -- below-right
                (i,j-1),   -- left
                (i,j+1)    -- right
                ]

-- This is just a utility function so that we can write the win-function easily.
-- Simply: positionGraph cs
-- takes the vertices in cs and puts an edge between them if they are
-- neighbours in the hexagonal grid.
positionGraph :: Set.Set (Int, Int) -> (Graph.Graph, Graph.Vertex -> (Int, Int))
positionGraph cs = 
  let nkks = [((i,j), -- node data
               (i,j), -- key of node
               Set.toList $ cs `Set.intersection` neighbours (i,j)) -- keys of neighbours in cs
             |(i,j) <- Set.toList cs]
      (graph, nkksFromVertex, keyToVertex) = Graph.graphFromEdges nkks
      keyFromVertex vtx = let (_, key, _) = nkksFromVertex vtx in key
  in (graph, keyFromVertex)
      
win nrows ncols vs = 
  let (gidxs, idxToVert) = positionGraph vs
      comps = map (map idxToVert) $ map Tree.flatten $ Graph.components gidxs in -- components
  or $ map (winningComp nrows) comps
  where
  winningComp nrows comp = 
    let rows = map fst comp in
    0 `elem` rows && (nrows-1) `elem` rows





-- ~~~~~ Properties, for automated testing ~~~~~~~~~~


-- TODO: This yields some pretty disconnected graphs.
-- In order to get nicer graphs, one could generate the vertices in a
-- box with some given dimensions.
instance (Arbitrary a, Ord a) => Arbitrary (Set.Set a) where
  arbitrary = do
    xs <- arbitrary
    return $ Set.fromList xs


-- We have a hexagonal net, so nodes better have
-- exactly 6 unique neighbours.
prop_neighbours :: (Int, Int) -> Bool
prop_neighbours (i,j) = 
  (Set.size $ neighbours (i,j)) == 6
  

prop_positionGraph :: Set.Set (Int, Int) -> Bool
prop_positionGraph cs = 
  let (gcs,_) = positionGraph cs in -- make graph grom cs
  Set.size cs == (length $ Graph.vertices gcs)