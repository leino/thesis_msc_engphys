-- This program reads results given by detailedquery.hs and makes them into LaTeX code
-- specially formatted for this thesis.
-- Error checking is virtually non-existant.

import System.Environment (getArgs)
import Text.Trifecta
import Text.Trifecta.Parser
import Text.Trifecta.Delta (Delta (..))
import Control.Applicative

data Winner = First | Neither deriving (Show, Read)

newtype Column = Column (Maybe (String, String, String)) deriving Show

newtype Row = Row [Column] deriving Show

-- holds a single table in the results file
data Table = Table { winner :: Winner, rows :: [Row] } deriving Show

-- holds all of the tables in the results file
newtype ResultsFile = ResultsFile [Table]

parseWinner :: Parser Winner
parseWinner = read <$> ( choice $ map (string . show) [First, Neither] )

resultsTable :: Parser Table
resultsTable = do
  -- header
  string "winner: "
  w <- parseWinner
  newline
  -- skip the "header column" containing column numbers  
  nonemptyLine
  -- the rows containing data
  (some $ row) >>= return . Table w
  where
    nonemptyLine = (some $ noneOf ['\n']) >> newline
    column = do
      choice [
        try $ (\[a,b,c] -> Column $ Just (a,b,c)) <$> (count 3 entry), -- try to parse data column
        (optional $ many $ char ' ') >> (return $ Column Nothing) -- try to parse empty column
        ]
      where
        entry = do
          optional space
          s <- some $ digit <|> char '.'
          optional space
          optional $ char ','
          optional space
          return $ s
    row = do
      -- skip first column (contains only row number)
      manyTill (noneOf ['\n']) (char '|')
      -- read in actual data columns
      cs <- column `sepEndBy1` (char '|')
      newline
      return $ Row cs
  
resultsFileParser :: Parser ResultsFile
resultsFileParser = ResultsFile <$> resultsTable `sepEndBy1` newline

tableToLatex :: Table -> String
tableToLatex tbl = ""

parse :: Parser a -> String -> Result a
parse p s = parseString p (Columns 0 0) s

main = do
  [resultsFile] <- getArgs
  contents <- readFile resultsFile
  case parse resultsFileParser contents of
    Success (ResultsFile tables) -> do
      print $ head tables
    Failure err -> print err
  