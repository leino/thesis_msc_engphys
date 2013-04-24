module HypergraphProcessing
       (batchProcessHypergraphs)
       where


import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import Data.List (intercalate)
import GHC.IO.Handle
import System.Process
import Control.Monad (unless)
import Data.Functor ((<$>))


    
showHypergraph :: Show a => Set.Set (Set.Set a) -> String
showHypergraph ss =
  let showSet :: (a -> String) -> [a] -> String
      showSet show xs = "{" ++ ( intercalate "," (map show xs) ) ++ "}"
      xss = (map Set.toList $ Set.toList ss) in
      showSet (showSet show) xss


-- helper function for processing the neighour graphs
winningSetsFromEdgeList :: Int -> [(Int, [Int])] -> Set.Set (Set.Set Int)
winningSetsFromEdgeList n =
  Set.fromList .          -- convert the list of sets to a set of sets
  map Set.fromList .      -- convert hyperedges to sets
  filter (not . null) .   -- filter out hyperedges with no vertices
  map snd .               -- throw away their names
  drop n                  -- we need only the nodes in the second class (i.e. those representing hyperedges)

edgeLists :: GenParser Char st [(Int, [Int])]
edgeLists = do
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
    char '\n'
    return node
  return ls
  where
    natural = read <$> many1 digit

parseEdgeLists :: Int -> String -> Either ParseError [ Set.Set (Set.Set Int) ]
parseEdgeLists numVerts output =
  map (winningSetsFromEdgeList numVerts) <$> parse (many1 edgeLists) "" output



batchProcessHypergraphs genbgPath showgPath batchSize numVerts numEdges nautyArgs hypergraphBatchProc = do
  let genproc =
        proc genbgPath
             ( ("-z"):("-q"):(show numVerts):(show numEdges):nautyArgs )
      process processHypergraphs g6s = do
        output <- readProcess showgPath ["-q"] (unlines g6s)
        case map showHypergraph <$> (parseEdgeLists numVerts output) of
          Left err -> error $ show err
          Right rs -> processHypergraphs g6s rs
  batchProcess batchSize genproc (process hypergraphBatchProc)


getAtMostNLines :: Int -> Handle -> IO [String]
getAtMostNLines 0 h = return []
getAtMostNLines n h = do
  eof <- hIsEOF h
  case eof of
    False -> do
      l <- hGetLine h
      ls <- getAtMostNLines (n-1) h
      return $ l:ls
    True -> return []

batchProcess batchSize genproc process = do
  let processAllLines h = do
        ls <- getAtMostNLines batchSize h
        unless (null ls) $ do
          process ls
          processAllLines h
  (_, Just hout, _, _) <- createProcess genproc{std_out = CreatePipe}
  processAllLines hout
  return ()
