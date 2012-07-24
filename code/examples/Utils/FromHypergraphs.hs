module Utils.FromHypergraphs
       (fromFile)
       where

import qualified Data.Set as Set
import Text.Trifecta.Parser
import GameTheory.PoGa.SetGame


setOf :: (MonadParser p, Ord x) => p x -> p (Set.Set x)
setOf p = do
  xs <- between (char '{') (char '}') ( p `sepBy` (char ',') )
  return $ Set.fromList xs
  
hypergraph :: MonadParser p => p ( Set.Set (Set.Set Int) )
hypergraph = 
  let natural' = fmap fromInteger natural :: MonadParser p => p Int in
  setOf (setOf natural')

fromHypergraph :: (Set.Set (Set.Set Int)) -> SetGame Int
fromHypergraph wss = 
    let board = Set.unions $ Set.toList wss in
    fromWinningSets board wss wss

fromFile :: String -> IO [SetGame Int]
fromFile fname = do
  Just wss <- parseFromFile (many $ do {h <- hypergraph; newline; return h}) fname
  return $ map fromHypergraph wss