{-# LANGUAGE FlexibleInstances #-}


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.GraphicalGame
import GameTheory.PoGa
import Control.Concurrent
import Prelude hiding (lines)

import qualified Data.Set as Set

n = 3 -- Tic-Tac-Toe board dimensions

mctsSetGame sg = Unexplored sg


extractPosition (Unexplored pos) = pos
extractPosition (Explored pos _ _ _) = pos

main = do
  let randomStrat = (liftRandomStrategy randomStrategy)
      interactiveStrat = (interactiveStrategy graphicalTicTacToe)
      --mctsStrat = (liftRandomStrategy (mctsStrategy 500))
      -- perfectSecondStrat = (liftPureStrategy secondStrategyPerfect)
      --perfectFirstStrat = (liftPureStrategy firstStrategyPerfect)
  ws <- runGraphicalTournament (replicate 10 graphicalTicTacToe) randomStrat interactiveStrat
  print ws
  putStrLn $ "first wins: " ++ (show $ length $ filter ((==) (Only First)) ws)
  putStrLn $ "second wins: " ++ (show $ length $ filter ((==) (Only Second)) ws)
  putStrLn $ "neither wins: " ++ (show $ length $ filter ((==) Neither) ws)

graphicalTicTacToe = GraphicalGame {name = "Tic-Tac-Toe",
                                    game = Game $ mctsSetGame $ ticTacToe n, 
                                    renderBoard = renderTicTacToeBoard n n}



renderTicTacToeBoard nrows ncols sg =
  renderGridBoard nrows ncols (firstChoices $ extractPosition sg)
                              (secondChoices $ extractPosition sg)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

renderGridBoard nrows ncols redPositions bluePositions = do  
  loadIdentity
  -- start by rendering the board as a white grid
  color $ Color3 1.0 1.0 (1.0::Double)
  grid nrows ncols
  -- next render the players vertices:
  pointSize $= 10.0
  let fstvtxs = Prelude.map vertexCoords (Set.toList $ redPositions) -- points representing first player's vertices
      sndvtxs = Prelude.map vertexCoords (Set.toList $ bluePositions) -- points representing second player's vertices
  -- first player's points are red
  color $ Color3 1.0 0.0 (0.0::Double)
  renderPrimitive Points $ sequence_ [vertex $ Vertex2 x y | (x,y) <- fstvtxs]
  -- second players points are blue
  color $ Color3 0.0 0.0 (1.0::Double)
  renderPrimitive Points $ sequence_ [vertex $ Vertex2 x y | (x,y) <- sndvtxs]
  where
  -- translate indicies of
  -- vertex to it's geometrical point
  vertexCoords :: (Int, Int) -> (Double, Double)  
  vertexCoords (i, j) =
    (-0.5 + (fromIntegral i+0.5)/(fromIntegral nrows),
      0.5 - (fromIntegral j+0.5)/(fromIntegral ncols))

-- Draw a grid of lines with m evenly spaced horizontal lines
-- and n evenly spaced vertical lines inside the square ([-0.5,0.5]x[-0.5,0.5])
grid :: Int -> Int -> IO ()
grid nrows ncols = do
  lines nrows
  rotate 90.0 (Vector3 0.0 0.0 (1.0::Double))
  lines ncols

-- Draw m horizontal lines, evenly vertically
-- spaced in the intervall [-0.5, 0.5]
lines :: Int -> IO ()
lines n
  | n <= 0 = return ()
  | otherwise = do
    renderPrimitive Lines $ horizLines
    where
    horizLines = sequence_ [do let y = (fromIntegral i / fromIntegral n) :: Double
                               vertex $ Vertex2 (-0.5) (0.5 - y)
                               vertex $ Vertex2 0.5 (0.5 - y)
                           | i <- [0 .. n]]