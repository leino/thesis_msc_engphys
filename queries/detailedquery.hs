{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Posix.Env (putEnv)
import Data.Convertible.Base
import Data.List (transpose)
import Control.Applicative
import Text.Printf
import System.Environment (getArgs)
import Data.List (find)
import Data.Maybe
import qualified Data.Set as Set

showTriple (x,y,z) =
  let showFloat = (printf "%.2f") in
  showFloat x ++ ", " ++ showFloat y ++ ", " ++ showFloat z

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
    
    
showAsTable rs =
  ("#2-edges\\#3-edges":(map show js)):(zipWith (:) (map show is) $ map (map $ "" `maybe` showTriple) $ buildMatrix rs)
  where
    (is', js') = unzip . fst . unzip $ rs
    (is, js) = let f = Set.toList . Set.fromList in (f is', f js')
    lookup :: Eq a => a -> [(a, b)] -> Maybe b
    lookup x xys = snd <$> find ((==) x . fst) xys
    buildMatrix rs = 
      [[
          lookup (i,j) rs
       | j <- js
       ]
      | i <- is
      ]
      
    
    
query winner numiterations =
  "SELECT num_two_sets, num_three_sets, SUM(num_first_wins), SUM(num_second_wins), SUM(num_neither_wins) \
  \FROM hypergraphs NATURAL JOIN results_Perfect_vs_Perfect NATURAL JOIN hypergraph_structure NATURAL JOIN results_UCT_vs_Perfect \
  \WHERE winner = '"++ winner ++ "' AND num_iterations_first = " ++ show numiterations ++ " GROUP BY num_two_sets, num_three_sets"

cols = ["#edges of size 2", "#edges of size 3", "#First wins", "#Second wins", "#Neither wins"]    

processRow :: [Maybe Int] -> Maybe ((Int, Int), (Float, Float, Float))
processRow cs@[mn2e, mn3e, mf, ms, mn] = do
  [n2e, n3e, f, s, n] <- sequence cs
  let sum = (fromIntegral $ f + s + n)
  return ((n2e, n3e), ((fromIntegral f)/sum, (fromIntegral s)/sum, (fromIntegral n)/sum))

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
      let runQuery winner niter = do
            Just rows <- sequence . map (processRow . map maybeFromSql) <$> quickQuery' connection (query winner niter) []
            putStrLn $ "winner: " ++ winner
            putStrLn $ showTable $ showAsTable rows
      runQuery "First" 2
      runQuery "First" 7
      runQuery "First" 12
      runQuery "First" 17      
      runQuery "First" 22      
      runQuery "First" 27      
      runQuery "Neither" 2
      runQuery "Neither" 7
      runQuery "Neither" 12
      runQuery "Neither" 17      
      runQuery "Neither" 22      
      runQuery "Neither" 27
      disconnect connection
    _ -> do
      putStrLn "error: invalid arguments: expected a database filename as first and only argument"