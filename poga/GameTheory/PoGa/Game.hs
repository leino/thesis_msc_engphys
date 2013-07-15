module GameTheory.PoGa.Game
       (Player(First, Second),
        Game(..),
        Winner(..),
        Position(choices, winner, terminal, turn),
        playGame,
        playTournament,
        opponent)
        where

data Player = First | Second
  deriving (Eq, Show)
           
type Strategy m p = p -> m p

data Winner = Neither | Both | Only Player
  deriving (Eq, Show)


newtype Game p = Game {position :: p}


-- The typeclass Position provides a very abstract view of
-- positional games.
class Position p where
  choices :: p -> [p]     -- available branches at position p
  winner :: p -> Winner   -- Nothing on non-leaf nodes
  terminal :: p -> Bool   -- Can a move be made? (For instance if the board is full, the position is terminal)
  turn :: p -> Player

opponent :: Player -> Player
opponent First = Second
opponent Second = First


playGame :: (Position p, Monad m) => Game p -> Strategy m p -> Strategy m p -> m Winner
playGame (Game pos) firstStrategy secondStrategy
  | terminal pos = return $ winner pos
  | otherwise = do
      pos' <- firstStrategy pos
      playGame (Game pos') secondStrategy firstStrategy
      
playTournament :: (Monad m, Position p) => Int -> Game p -> Strategy m p -> Strategy m p -> m [Winner]
playTournament n game fststrat sndstrat = do
  let g = playGame game fststrat sndstrat
  ws <- sequence $ replicate n g
  return ws