import Prelude
import Text.Trifecta.Parser hiding (line)
import qualified Data.Set as Set
import Data.List (null)
import System.Process
import System.IO
import GHC.IO.Handle
import System.Environment (getArgs)
import GameTheory.PoGa.SetGame as SetGame
import Data.List (intersperse)

showHypergraph :: (Show v, Ord v) => Set.Set (Set.Set v) -> String
showHypergraph = 
  showSet (showSet show)
  where
  surround a b xs = [a] ++ xs ++ [b]
  showSet show =
    surround '{' '}' . concat . intersperse "," . map show . Set.toList

generate numVerts numEdges = do
  -- file names where the different stages will be output to
  let sessionName = "hg_" ++ show numVerts ++ "_" ++ show numEdges
      g6GraphsFileName = sessionName ++ "_" ++ "g6graphs" ++ ".txt"
      graphsFileName = sessionName ++ "_" ++ "graphs.txt"
      hypergraphsFileName = sessionName ++ "_" ++ "hypergraphs.txt"
  -- nauty directory
  let nautyDir = "./nauty24r2"
  -- first generate the compact but hard to understand graph6 format
  g6s <- readProcess (nautyDir ++ "/genbg") ["-z", "-q", show numVerts, show numEdges, "-d0:1"] ""
  writeFile g6GraphsFileName g6s
  gs <- readProcess (nautyDir ++ "/showg") ["-q"] g6s
  writeFile graphsFileName gs
  Just ess <- parseFromFile (some graph) graphsFileName
  let wss = map (winningSetsFromEdgeList numVerts) ess
  writeFile hypergraphsFileName $ unlines $ map showHypergraph wss
  where
  -- helper function for processing the neighour graphs
  winningSetsFromEdgeList :: Int -> [(Int, [Int])] -> Set.Set (Set.Set Int)
  winningSetsFromEdgeList n =
    Set.fromList .          -- convert the list of sets to a set of sets
    map Set.fromList .      -- convert hyperedges to sets
    filter (not . null) .   -- filter out hyperedges with no vertices
    map snd .               -- throw away their names
    drop n                  -- we need only the nodes in the second class (i.e. those representing hyperedges)

graph :: MonadParser m => m [(Int, [Int])]
graph = do
  n <- natural >>= return . fromInteger
  ls <- count n $ do
    node <- do
      spaces
      node <- natural >>= return . fromInteger
      spaces
      char ':'
      spaces
      neighbours <- natural `sepBy` spaces
      char ';'
      return (node, map fromInteger neighbours)
    newline
    return node
  return ls

main = do
  as <- getArgs
  let ns = map read as :: [Int]
  case ns of
    [m,n] -> generate m n
    _ -> error "print: you must give two arguments: number of vertices and number of edges"
