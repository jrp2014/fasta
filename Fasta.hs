module Fasta where

import           Control.Monad
import           Data.List.Unique
import qualified Data.Map         as Map

-- http://users.csc.calpoly.edu/~dekhtyar/448-Spring2013/lectures/lec08.448.pdf

type IndexList = [(String, [Int])]

type IndexMap = Map.Map String [Int]

type DiagonalScores = Map.Map Int Int

sigma :: String
sigma = "ATCG"

s1, t1 :: String
s1 = "ATCGTATCG"

t1 = "CAGATCGTCTCGAT"

els :: Int -> String -> [String]
els = replicateM

ktupleMap :: Int -> String -> IndexMap
ktupleMap k s = Map.fromListWith (++) $ ktuples k s

ktuples :: Int -> String -> [(String, [Int])]
ktuples k s
  | k < lens = ktuples' k 0 lens s
  | otherwise = undefined
  where
    lens = length s

ktuples' :: Int -> Int -> Int -> String -> IndexList
ktuples' k ix len s@(_:t) =
  if ix + k > len
    then []
    else (take k s, [ix]) : ktuples' k (ix + 1) len t
ktuples' _ _ _ _ = undefined

diagonals :: Int -> String -> String -> IndexMap
diagonals k s t = dotplot ls lt
  where
    ls = ktupleMap k s
    lt = ktupleMap k t
    dotplot :: IndexMap -> IndexMap -> IndexMap
    dotplot = Map.intersectionWith (\x y -> [i - j | i <- x, j <- y])

diagonalScores :: Int -> String -> String -> [(Int, Int)]
--diagonalScores k s t =  count $ foldr (++) [] diags
diagonalScores k s t = count $ concat diags
  where
    diags :: IndexMap
    diags = diagonals k s t

