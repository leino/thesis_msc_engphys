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

fromHypergraph :: Int -> (Set.Set (Set.Set Int)) -> SetGame Int
fromHypergraph numvertices wss = 
    let board = Set.fromList $ [0 .. numvertices-1] in
    fromWinningSets board wss wss

fromRepresentation :: Int -> String -> SetGame Int
fromRepresentation numvertices =
  fromHypergraph numvertices . winningSetsFromString

winningSetsFromString :: String -> (Set.Set (Set.Set Int))
winningSetsFromString string = 
    let Right wss = parse hypergraph "unknown" string in wss