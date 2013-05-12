module ExperimentDescription
       (Strategy (..), Experiment (..), parseStrategy, showKind) where

import Text.ParserCombinators.Parsec
import ParsecUtils

data Strategy = Perfect |
                UCT {numIterations :: Int}

data Experiment = Deterministic {firstStrategy :: Strategy, secondStrategy :: Strategy} |
                  Stochastic {firstStrategy :: Strategy, secondStrategy :: Strategy, numPlays :: Int}
                deriving Show

instance Show Strategy where
  show Perfect = "Perfect"
  show (UCT n) = concat ["UCT", show n]
  
showKind :: Strategy -> String
showKind Perfect = "Perfect"
showKind (UCT _) = "UCT"
  
parseStrategy :: String -> Either String Strategy
parseStrategy str = do
  let p = choice [string "Perfect" >> return Perfect,
                  string "UCT" >> natural >>= return . UCT]
      parseResult = parse p "" str
  case parseResult of
    Left parseError -> Left $ show $ parseError
    Right experiment -> return experiment
