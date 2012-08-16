module Graphics.Graph
       (graph)
       where

import qualified Graphics.UI.GLUT as GLUT
import System.Posix (forkProcess)

test = forkProcess $ displayGL $ GLUT.preservingMatrix $ barDiagram $ map (\x -> (sin x) ** 2.0) [1.0 .. 50.0]
-- ~~~~~~ Helper function ~~~~~~~~~~~~~~~~~~~


displayGL display = do
  GLUT.getArgsAndInitialize
  GLUT.createWindow ""
  GLUT.displayCallback GLUT.$= do
    GLUT.clear [GLUT.ColorBuffer]
    display
    GLUT.flush
  GLUT.mainLoop
  
color :: Double -> Double -> Double -> IO ()
color r g b = GLUT.color $ GLUT.Color3 r g b

vertex :: Double -> Double -> IO ()
vertex x y = GLUT.vertex $ GLUT.Vertex2 x y

scale :: Double -> Double -> IO ()
scale sx sy = GLUT.scale sx sy 1.0

translate :: Double -> Double -> IO ()
translate x y = GLUT.translate $ GLUT.Vector3 x y 0.0

myTriangle r g b = do
  color r g b
  GLUT.renderPrimitive GLUT.Triangles $ do
    sequence_ [vertex 0.0 0.0,
               vertex 0.0 1.0,
               vertex 1.0 0.0]


bars :: [Double] -> IO ()
bars [] = return ()
bars (x:xs) = do
  bar x
  translate 1.5 0.0
  bars xs
  where
    bar x = GLUT.preservingMatrix $ do
      GLUT.renderPrimitive GLUT.Quads $ do
        vertex 0.0 0.0
        vertex 1.0 0.0
        vertex 1.0 x
        vertex 0.0 x

barDiagram :: [Double] -> IO ()
barDiagram xs = GLUT.preservingMatrix $ do
  let n = length xs
      diagramWidth = 0.5 + (fromIntegral $ n-1)*1.5 + 0.5
      diagramHeight = maximum $ map abs xs
  translate (-1.0) (-1.0)
  scale (2.0/diagramWidth) (2.0/diagramHeight)
  bars xs

-- ~~~~~~~~  Exports  ~~~~~~~~~~~~~~~~~~~~~~~~~

graph :: [Double] -> IO ()
graph xs = displayGL $ barDiagram xs
  