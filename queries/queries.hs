{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Posix.Env (putEnv)
import Data.Convertible.Base
import Data.List (transpose)
import Control.Applicative
import Text.Printf
import System.Environment (getArgs)

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

cols_01 = ["#vertices", "#edges", "%First wins", "%Second wins", "%Neither wins", "#iterations"]
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

cols_02 = cols_01
query_02 = "SELECT numvertices, numedges, SUM(numfirstwins), SUM(numsecondwins), SUM(numneitherwins), numiterations \
           \FROM hypergraphs NATURAL JOIN perfect NATURAL JOIN mctsvsperfect \
           \WHERE winner = 'Neither' \
           \GROUP BY numvertices, numedges, numiterations"
maybeProcess_02 = maybeProcess_01
showRow_02 = showRow_01

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
  args <- getArgs
  case args of
    [databaseFileName] -> do
      putEnv "TMPDIR=/usr/tmp" -- put the temporary directory to some place with more storage than /tmp (at least useful on my system, uncomment if you don't have /usr/tmp)
      connection <- connectSqlite3 databaseFileName
      let queryAndPrint (query, colDescs, processRow, showRow) =  do
            sqlResults <- quickQuery' connection query []
            let results = map (map maybeFromSql) sqlResults
                maybeProcessedResults = map processRow results
            putStrLn $ showTable $ colDescs : (map showRow maybeProcessedResults)
      queryAndPrint (query_01, cols_01, maybeProcess_01, showRow_01)
      queryAndPrint (query_02, cols_02, maybeProcess_02, showRow_02)
      disconnect connection
    _ -> do
      putStrLn "Invalid arguments. Expecting a database filename."