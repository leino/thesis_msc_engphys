module FromRepresentation
       (winningSetsFromString, fromRepresentation)
       where

import Text.Parsec
import qualified Data.Set as Set
import GameTheory.PoGa.SetGame
import Data.Functor


setOf :: Ord a => Parsec String () a -> Parsec String () (Set.Set a)
setOf p = do
  xs <- between (char '{') (char '}') ( p `sepBy` (char ',') )
  return $ Set.fromList xs

hypergraph :: Parsec String () (Set.Set (Set.Set Int))
hypergraph = 
  setOf (setOf natural)
  where
    natural = (read <$> many1 digit) :: Parsec String () Int

fromHypergraph :: (Set.Set (Set.Set Int)) -> SetGame Int
fromHypergraph wss = 
    let board = Set.unions $ Set.toList wss in
    fromWinningSets board wss wss

fromRepresentation :: String -> SetGame Int
fromRepresentation  =
  fromHypergraph . winningSetsFromString

winningSetsFromString :: String -> (Set.Set (Set.Set Int))
winningSetsFromString cs = 
    let Right wss = parse hypergraph "unknown" representation in wss