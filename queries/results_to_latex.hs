-- This program reads results given by detailedquery.hs and makes them into LaTeX code
-- specially formatted for this thesis.
-- Error checking is virtually non-existant.

import System.Environment (getArgs)
import Text.Trifecta
import Text.Trifecta.Parser
import Text.Trifecta.Delta (Delta (..))
import Control.Applicative
import Data.List (transpose, intersperse)
import Control.Monad (sequence)

data Winner = First | Neither deriving (Show, Read, Eq)

newtype Entry = Entry { entry :: Maybe (String, String, String) } deriving Show

newtype Row = Row {entries :: [Entry]} deriving Show

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
        try $ (\[a,b,c] -> Entry $ Just (a,b,c)) <$> (count 3 entry), -- try to parse data column
        (optional $ many $ char ' ') >> (return $ Entry Nothing) -- try to parse empty column
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
      cs <- init <$> column `sepBy1` (char '|')
      newline
      return $ Row cs
  
resultsFileParser :: Parser ResultsFile
resultsFileParser = ResultsFile <$> resultsTable `sepEndBy1` newline

tableToLatex :: Table -> String
tableToLatex tbl = ""

parse :: Parser a -> String -> Result a
parse p s = parseString p (Columns 0 0) s

-- interleave rows from the tables
interleaveRows :: [Table] -> [[Row]]
interleaveRows = transpose . map rows

-- combine a list of rows to a single row of lists of entries
combineRows :: [Row] -> [ Maybe [(String, String, String)] ]
combineRows = map (sequence . map entry) . (transpose . map entries)

-- make a big result table from a list of tables
processTables :: [Table] -> [[Maybe [(String, String, String)]]]
processTables = map combineRows . interleaveRows

-- convert a single column of a result table row into latex
columnLatex :: Maybe [(String, String, String)] -> String
columnLatex Nothing = ""
columnLatex (Just triples) =
  concat . intersperse "\n" $
  [ header, 
    concat . intersperse "\\\\\n" . map tripleLatex $ triples,
    footer ]
  where
    header = "\\begin{tabular}{>{\\tiny\\ttfamily}c}" -- TODO: should be some kind of table
    footer = "\\end{tabular}"
    tripleLatex (a,b,c) = concat $ intersperse "," $ map convertNumber [a, b, c]

-- convert row of result table into latex
rowLatex :: (Int, [Maybe [(String, String, String)]]) -> String
rowLatex (i,cs) = concat . intersperse "\n&\n" . (++) [show i] . map columnLatex $ cs

-- conver the whole result table to latex
tableLatex :: [[Maybe [(String, String, String)]]] -> String
tableLatex rs =
  concat . intersperse "\n" $
  [ header,
    topRow,
    concat . map (flip (++) "\\\\\\hline\n") . map rowLatex $ zip [0 ..] rs,
    footer ]
  where
    numColumns = length $ head rs
    upperRightCorner = concat ["\\begin{tabular}{>{\\tiny\\ttfamily}c}",
                               "\\#3-edges \\\\\\hline\n",
                               "\\#2-edges \\\\\n",
                               "\\end{tabular}"]
    topRow = (concat $ intersperse "&" $ [upperRightCorner] ++ map show [0 .. numColumns-1]) ++ "\\\\\\hline\n"
    firstColumnDescriptor = ">{\\small\\ttfamily}r|"
    columnDescriptor = ">{\\small\\ttfamily}c|"
    columnDescriptors = concat ["{", "|", firstColumnDescriptor,
                                concat $ replicate numColumns columnDescriptor, "}"]
    header = concat $ intersperse "\n" $ ["\\begin{landscape}",
                                          "\\bgroup",
                                          "\\setlength{\\tabcolsep}{.16em}",
                                          "\\def\\arraystretch{0.5}",
                                          "\\begin{table}",
                                          "\\begin{tabular}" ++ columnDescriptors,
                                          "\\hline"
                                          ]
    footer = concat $ intersperse "\n" $ ["%\\hline",
                                          "\\end{tabular}",
                                          "\\end{table}",
                                          "\\egroup",
                                          "\\end{landscape}"]

-- converts number to the format we want
convertNumber :: String -> String
convertNumber "1.00" = "100"
convertNumber ('0':'.':'0':b) = b
convertNumber ('0':'.':a:b) = a:b

main = do
  [resultsFile] <- getArgs
  contents <- readFile resultsFile
  case parse resultsFileParser contents of
    Success (ResultsFile tables) -> do
      let firstWinnerTables = filter ((==) First . winner) tables
          neitherWinnerTables = filter ((==) Neither . winner) tables
      writeFile "firstWinnerTablesLatex.txt" $ tableLatex $ processTables firstWinnerTables
      writeFile "neitherWinnerTablesLatex.txt" $ tableLatex $ processTables neitherWinnerTables
    Failure err -> print err
  