import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.GraphicalGame
import GameTheory.PoGa
import Control.Monad (when)
import qualified Data.Set as Set



(nrows, ncols) = (3,3)::(Int,Int) -- Hex board dimensions

graphicalHex = GraphicalGame {name = "Hex",
                              game = Game $ hex nrows ncols,
                              renderBoard = renderHexBoard nrows ncols}

main = runGraphicalGame graphicalHex
                        (interactiveStrategy graphicalHex)
                        (liftPureStrategy secondStrategyPerfect)



-- ~~~~~~~~ Support functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


renderHexBoard :: Int -> Int -> SetGame (Int, Int) -> IO ()
renderHexBoard nrows ncols sg = do
  let (gridWidth, gridHeight) = ((fromIntegral$ncols-1) + 
                                 (cos$pi/3.0)*(fromIntegral$nrows-1),
                                 (sin$pi/3.0)*(fromIntegral$nrows-1))
  loadIdentity
  color $ Color3 1.0 1.0 (1.0::Double)
  hexnet nrows ncols
  -- time to render the players vertices
  pointSize $= 5.0
  let idxToPt (i,j) = (fromIntegral j + (cos$pi/3.0) * fromIntegral i,
                       (sin$pi/3.0) * fromIntegral i) :: (Double, Double)
      idxs = [(i,j) | i <- [0 .. nrows-1], j <- [0 .. ncols-1]]
  translate $ Vector3 (-0.5) (-0.5) (0.0::Double)
  scale (1.0/gridWidth) (1.0/gridHeight) (1.0::Double)
  color $ Color3 1.0 0.0 (0.0::Double) -- first player is red
  renderPrimitive Points $ sequence_ [vertex $ Vertex2 x y
                                     |(x,y) <- map idxToPt (Set.toList $ firstChoices sg)]
  color $ Color3 0.0 0.0 (1.0::Double) -- second player is blue
  renderPrimitive Points $ sequence_ [vertex $ Vertex2 x y
                                     |(x,y) <- map idxToPt (Set.toList $ secondChoices sg)]

-- Render a "hex-net" so that the 
-- bottom-left center is at (-0.5,-0.5)
-- and the top-right center is at (0.5,0.5)
hexnet :: Int -> Int -> IO ()
hexnet nrows ncols = preservingMatrix $ do
  -- We decide that the center points of neighbouring cells
  -- need to be a distance of 1.0 apart:
  let neighbourCenterDist = 1.0
  -- Then the entire geometry of the structure is determined:
      sideLength = neighbourCenterDist * (tan $ pi/6.0) :: Double
  -- Here are some utility and drawing functions
  let rotateCCW :: Double -> IO ()  
      rotateCCW degs = rotate degs $ Vector3 0.0 0.0 1.0
  -- vside draws a vertical side of length sideLength at x=0.5
  -- from the current position:
      vside = do
        preservingMatrix $ do
          translate $ Vector3 0.5 0.0 (0.0::Double)
          renderPrimitive Lines $ sequence_ [vertex $ Vertex2 (0.0::Double) (-sideLength / 2.0),
                                             vertex $ Vertex2 (0.0::Double) ( sideLength / 2.0)]
  -- Scale the vertices to fit in [-0.5,0.5]^2
  let (gridWidth, gridHeight) = (neighbourCenterDist*(fromIntegral$ncols-1) + 
                                 (cos$pi/3.0)*(fromIntegral$nrows-1),
                                 (sin$pi/3.0)*(fromIntegral$nrows-1))
  scale (1.0/gridWidth) (1.0/gridHeight) (0.0::Double)
  -- Center:
  translate $ Vector3 (-gridWidth/2.0) (-gridHeight/2.0) (0.0::Double)
  sequence_ [preservingMatrix $ do
               let (x,y) = (fromIntegral i, fromIntegral j)
                   -- The sides of a hexagon
                   [rightSide,
                    aboveRightSide,
                    aboveLeftSide,
                    leftSide,
                    belowLeftSide,
                    belowRightSide] = [preservingMatrix $ rotateCCW (k*60.0) >> vside
                                      |k <- [0 .. 5]]
               -- go to center of current cell
               translate $ Vector3 (y*(cos$pi/3.0) + x) (y*(sin$pi/3.0)) (0.0::Double)
               -- In:
               -- * an internal cell,
               -- * a cell on the left side,
               -- * or a cell on the bottom side,
               -- we only need to render:
               leftSide >> belowLeftSide >> aboveLeftSide >> belowRightSide
               -- In a cell on top edge or right edge, we need also:
               when (i == ncols-1) $ rightSide
               when (j == nrows-1) $ aboveRightSide

            |i <- [0 .. ncols-1], j <- [0 .. nrows-1]]