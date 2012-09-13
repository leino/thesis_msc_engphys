import HypergraphProcessing
import System (getArgs)

printResults numVerts numEdges nautyArgs = do
  let printBatch g6s rs = putStr . unlines . map show $ zip g6s rs
  batchProcessHypergraphs 2048 numVerts numEdges nautyArgs printBatch 
  
  
main = do
  args <- getArgs
  case args of
    numVertsStr:numEdgesStr:nautyArgs -> do
      let (numVerts, numEdges) = (read numVertsStr, read numEdgesStr) :: (Int, Int)
      printResults numVerts numEdges nautyArgs
    _ -> putStrLn "incorrect arguments"
