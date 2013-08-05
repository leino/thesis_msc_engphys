module GameTheory.PoGa.Strategy.Random
       (randomStrategy)
       where

import GameTheory.PoGa.Game
import qualified Control.Monad.Random as Random

-- sample uniformly from all choices
randomStrategy :: (Random.MonadRandom m, Position p) => p -> m p
randomStrategy pos = Random.fromList [(c,1) | c <- choices pos]