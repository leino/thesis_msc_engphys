{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Posix.Env (putEnv)
import Data.Convertible.Base
import Data.List (transpose)
import Control.Applicative
import Text.Printf

-- columWidths rows gives the widths of the columns in the matrix with the given rows
columnWidths :: [[String]] -> [Int]
columnWidths = map (maximum . map length) . transpose

showTable :: [[String]] -> String
showTable [] = ""
showTable rs = 
  let ws = columnWidths rs in
  unlines [concat [showCell w c ++ " | " | (w,c) <- zip ws cs]
          |cs<-rs]
  where
    showCell w s = (replicate (w - length s) ' ') ++ s

cols_01 = ["vertices", "edges", "First wins", "Second wins", "Neither wins", "iterations"]
query_01 = "SELECT numvertices, numedges, SUM(numfirstwins), SUM(numsecondwins), SUM(numneitherwins), numiterations \
           \FROM hypergraphs NATURAL JOIN perfect NATURAL JOIN mctsvsperfect \
           \WHERE winner = 'First' \
           \GROUP BY numvertices, numedges, numiterations"
maybeProcess_01 :: [Maybe Int] -> (Maybe Int, Maybe Int, Maybe Float, Maybe Float, Maybe Float, Maybe Int)
maybeProcess_01 [v, e, f, s, n, i] = 
  let maybeSum = (\x y z -> x+y+z) <$> f <*> s <*> n in
  (v, e, (/) <$> (fromIntegral <$> f) <*> (fromIntegral <$> maybeSum),
         (/) <$> (fromIntegral <$> s) <*> (fromIntegral <$> maybeSum),
         (/) <$> (fromIntegral <$> n) <*> (fromIntegral <$> maybeSum),
         i)
showRow_01 :: (Maybe Int, Maybe Int, Maybe Float, Maybe Float, Maybe Float, Maybe Int) -> [String]
showRow_01 (v, e, f, s, n, i) = 
  [maybeShowInt v, maybeShowInt e, maybeShowFloat f, maybeShowFloat s, maybeShowFloat n, maybeShowInt i]
  where
    maybeShowInt :: Maybe Int -> String
    maybeShowInt = "" `maybe` show
    maybeShowFloat :: Maybe Float -> String
    maybeShowFloat = "" `maybe` (printf "%.2f")

cols_02 = ["vertices", "edges", "First wins", "Second wins", "Neither wins", "iterations"]
query_02 = "SELECT numvertices, numedges, SUM(numfirstwins), SUM(numsecondwins), SUM(numneitherwins), numiterations \
           \FROM hypergraphs NATURAL JOIN perfect NATURAL JOIN mctsvsperfect \
           \WHERE winner = 'First' \
           \GROUP BY numvertices, numedges, numiterations"

showSql :: SqlValue -> String
showSql SqlNull = ""
showSql sql = 
  case safeFromSql sql of 
    Left e -> show sql
    Right s -> s
    
maybeFromSql :: Convertible SqlValue a => SqlValue -> Maybe a
maybeFromSql sqlVal =
  case safeFromSql sqlVal of
    Left _ -> Nothing
    Right val -> val

main = do
  putEnv "TMPDIR=/usr/tmp"
  connection <- connectSqlite3 "../data/twothree_accum-2012-12-07.db"
  sqlResults <- quickQuery' connection query_01 []
  let results = map (map maybeFromSql) sqlResults
      maybeProcessedResults = map maybeProcess_01 results
  putStrLn $ showTable $ map showRow_01 maybeProcessedResults
  disconnect connection