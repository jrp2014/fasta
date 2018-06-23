module Main where

import Fasta 

main :: IO ()
main = do
  print $ ktuples 3 s1
  print $ ktupleMap 3 s1
  print $ ktupleMap 3 t1
  print $ diagonals 3 s1 t1
  print $ diagonalScores 3 s1 t1
