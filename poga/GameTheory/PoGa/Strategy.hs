module GameTheory.PoGa.Strategy
       (Strategy,
        module GameTheory.PoGa.Strategy.Random,
        module GameTheory.PoGa.Strategy.Perfect,
        module GameTheory.PoGa.Strategy.MCTS
       )
       where

import GameTheory.PoGa.Strategy.Random
import GameTheory.PoGa.Strategy.Perfect
import GameTheory.PoGa.Strategy.MCTS


import Data.Function (on)
import GameTheory.PoGa.Game as Game
import qualified Data.Set as Set

type Strategy m p = p -> m p