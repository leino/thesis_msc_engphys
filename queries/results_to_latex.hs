-- This program reads results given by detailedquery.hs and makes them into LaTeX code
-- specially formatted for this thesis.
-- Error checking is virtually non-existant.

import System.Environment (getArgs)
import Text.Trifecta
import Text.Trifecta.Parser
import Text.Trifecta.Delta (Delta (..))
import Control.Applicative ((<$>))

data Winner = First | Neither deriving (Show, Read)

-- holds a single table in the results file
data Table = Table { winner :: Winner, rows :: [String] } deriving Show

-- holds all of the tables in the results file
data ResultsFile = ResultsFile [Table]

parseWinner :: Parser Winner
parseWinner = read <$> ( choice $ map (string . show) [First, Neither] )

resultsTable :: Parser Table
resultsTable = do
  -- header
  string "winner: "
  w <- parseWinner
  newline
  let nonemptyLine = do
        s <- some $ noneOf ['\n']
        newline
        return s
  (some $ nonemptyLine) >>= return . Table w

resultsFileParser :: Parser ResultsFile
resultsFileParser = ResultsFile <$> resultsTable `sepEndBy1` newline

tableToLatex :: Table -> String
tableToLatex tbl = ""

parse :: Parser a -> String -> Result a
parse p s = parseString p (Columns 0 0) s

main = do
  [resultsFile] <- getArgs
  contents <- readFile resultsFile
  let Success (ResultsFile tables) = parse resultsFileParser contents
  print $ head tables
  