{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Posix.Env (putEnv)
import Data.Convertible.Base
import Data.List (transpose)
import Control.Applicative
import Text.Printf
import System.Environment (getArgs)


maybeShowInt :: Maybe Int -> String
maybeShowInt = "" `maybe` show
maybeShowFloat :: Maybe Float -> String
maybeShowFloat = "" `maybe` (printf "%.2f")

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

cols_02 = cols_01
query_02 = "SELECT numvertices, numedges, SUM(numfirstwins), SUM(numsecondwins), SUM(numneitherwins), numiterations \
           \FROM hypergraphs NATURAL JOIN perfect NATURAL JOIN mctsvsperfect \
           \WHERE winner = 'Neither' \
           \GROUP BY numvertices, numedges, numiterations"
maybeProcess_02 = maybeProcess_01
showRow_02 = showRow_01



query_03 = "SELECT hc.numvertices, hc.numedges, IFNULL(numfirstwins, 0), IFNULL(numsecondwins, 0), IFNULL(numneitherwins, 0) \
           \FROM (SELECT numvertices, numedges FROM hypergraphs GROUP BY numvertices, numedges) hc \
           \LEFT OUTER JOIN \
           \(SELECT numvertices, numedges, count(*) AS numfirstwins FROM hypergraphs NATURAL JOIN perfect WHERE winner = 'First' GROUP BY numvertices, numedges) fwc \
           \ON (hc.numvertices = fwc.numvertices AND hc.numedges = fwc.numedges) \
           \LEFT OUTER JOIN \
           \(SELECT numvertices, numedges, count(*) AS numsecondwins FROM hypergraphs NATURAL JOIN perfect WHERE winner = 'Second' GROUP BY numvertices, numedges) swc \
           \ON (hc.numvertices = swc.numvertices AND hc.numedges = swc.numedges) \
           \LEFT OUTER JOIN \
           \(SELECT numvertices, numedges, count(*) AS numneitherwins FROM hypergraphs NATURAL JOIN perfect WHERE winner = 'Neither' GROUP BY numvertices, numedges) nwc \
           \ON (hc.numvertices = nwc.numvertices AND hc.numedges = nwc.numedges);"
cols_03 = ["#vertices", "#edges", "#First wins", "#Second wins", "#Neither wins"]
maybeProcess_03 :: [Maybe Int] -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
maybeProcess_03 [v, e, f, s, n] = (v, e, f, s, n)
showRow_03 :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> [String]
showRow_03 (v, e, f, s, n) = 
  [maybeShowInt v, maybeShowInt e, maybeShowInt f, maybeShowInt s, maybeShowInt n]


minEdgeQuery winner numiterations = 
  let query = "SELECT numedges, min_edge_size, SUM(numfirstwins), SUM(numsecondwins), SUM(numneitherwins) \
              \FROM hypergraphs NATURAL JOIN perfect NATURAL JOIN hypergraph_structure NATURAL JOIN mctsvsperfect \
              \WHERE winner = '"++ winner ++ "' AND numiterations = " ++ show numiterations ++ " GROUP BY numedges, min_edge_size"
      cols = ["#edges", "min edge size", "#First wins", "#Second wins", "#Neither wins"]
      maybeProcess :: [Maybe Int] -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
      maybeProcess [e, m, f, s, n] = (e, m, f, s, n)
      showRow :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> [String]
      showRow (e, m, f, s, n) = 
        [maybeShowInt e, maybeShowInt m, maybeShowInt f, maybeShowInt s, maybeShowInt n] in
  (query, cols, maybeProcess, showRow)
  where
    maybeShowInt :: Maybe Int -> String
    maybeShowInt = "" `maybe` show
    maybeShowFloat :: Maybe Float -> String
    maybeShowFloat = "" `maybe` (printf "%.2f")

detailedQuery winner numiterations = 
  let query = "SELECT num_two_edges, num_three_edges, SUM(numfirstwins), SUM(numsecondwins), SUM(numneitherwins) \
              \FROM hypergraphs NATURAL JOIN perfect NATURAL JOIN hypergraph_structure NATURAL JOIN mctsvsperfect \
              \WHERE winner = '"++ winner ++ "' AND numiterations = " ++ show numiterations ++ " GROUP BY num_two_edges, num_three_edges"
      cols = ["#edges of size 2", "#edges of size 3", "#First wins", "#Second wins", "#Neither wins"]
      maybeProcess :: [Maybe Int] -> (Maybe Int, Maybe Int, Maybe Float, Maybe Float, Maybe Float)
      maybeProcess [n2e, n3e, f, s, n] = 
        let maybeSum = (\x y z -> x+y+z) <$> f <*> s <*> n in
        (n2e, n3e, (/) <$> (fromIntegral <$> f) <*> (fromIntegral <$> maybeSum),
                   (/) <$> (fromIntegral <$> s) <*> (fromIntegral <$> maybeSum),
                   (/) <$> (fromIntegral <$> n) <*> (fromIntegral <$> maybeSum) )
      showRow :: (Maybe Int, Maybe Int, Maybe Float, Maybe Float, Maybe Float) -> [String]
      showRow (n2e, n3e, f, s, n) = 
        [maybeShowInt n2e, maybeShowInt n3e, maybeShowFloat f, maybeShowFloat s, maybeShowFloat n] in
  (query, cols, maybeProcess, showRow)
  where
    maybeShowInt :: Maybe Int -> String
    maybeShowInt = "" `maybe` show
    maybeShowFloat :: Maybe Float -> String
    maybeShowFloat = "" `maybe` (printf "%.2f")

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
      --putStrLn $ "First"
      --queryAndPrint (query_01, cols_01, maybeProcess_01, showRow_01)
      --putStrLn $ "Neither"
      --queryAndPrint (query_02, cols_02, maybeProcess_02, showRow_02)
      --queryAndPrint (query_03, cols_03, maybeProcess_03, showRow_03)
      -- putStrLn $ "First, 10 iterations"
      -- queryAndPrint $ minEdgeQuery "First" 10
      -- putStrLn $ "First, 20 iterations"
      -- queryAndPrint $ minEdgeQuery "First" 20
      -- putStrLn $ "First, 30 iterations"      
      -- queryAndPrint $ minEdgeQuery "First" 30
      -- putStrLn $ "Neither, 10 iterations"      
      -- queryAndPrint $ minEdgeQuery "Neither" 10
      -- putStrLn $ "Neither, 20 iterations"      
      -- queryAndPrint $ minEdgeQuery "Neither" 20
      -- putStrLn $ "Neither, 30 iterations"      
      -- queryAndPrint $ minEdgeQuery "Neither" 30
            
      putStrLn $ "First, 10 iterations"
      queryAndPrint $ detailedQuery "First" 10
      putStrLn $ "First, 20 iterations"
      queryAndPrint $ detailedQuery "First" 20
      putStrLn $ "First, 30 iterations"      
      queryAndPrint $ detailedQuery "First" 30
      putStrLn $ "Neither, 10 iterations"      
      queryAndPrint $ detailedQuery "Neither" 10
      putStrLn $ "Neither, 20 iterations"      
      queryAndPrint $ detailedQuery "Neither" 20
      putStrLn $ "Neither, 30 iterations"      
      queryAndPrint $ detailedQuery "Neither" 30
      
      disconnect connection
    _ -> do
      putStrLn "Invalid arguments. Expecting a database filename."