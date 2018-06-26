module Fasta where

import Control.Monad
import Data.List
import Data.List.Unique
import qualified Data.Map as Map

-- http://users.csc.calpoly.edu/~dekhtyar/448-Spring2013/lectures/lec08.448.pdf
type Ix = Int -- String index

type DiagIx = Int -- Diagonal index

type Score = Int

type IndexList = [(String, [Ix])]

type IndexMap = Map.Map String [Ix]

type DiagMap = Map.Map String [DiagIx]

type DiagonalScores = Map.Map Int Int

type Subst = Bool -> Score

type Tally = (Int, [Int], Int) -- running tally of matches, previous match lengths, gap count

sigma :: String
sigma = "ATCG"

s1, t1 :: String
s1 = "ATCGTATCG"

t1 = "CAGATCGTCTCGAT"

matchScore :: Score
matchScore = 5

gapScore :: Score
gapScore = -4

els :: Int -> String -> [String]
els = replicateM

ktupleMap :: Int -> String -> IndexMap
ktupleMap k s = Map.fromListWith (++) $ ktuples k s

ktuples :: Int -> String -> [(String, [Ix])]
ktuples k s
  | k < lens = ktuples' 0 lens s
  | otherwise = undefined
  where
    lens = length s
    -- TODO:: think about whether this is better done with a traverse
    ktuples' :: Int -> Int -> String -> IndexList
    ktuples' ix len s'@(_:t) =
      if ix + k > len
        then []
        else (take k s', [ix]) : ktuples' (ix + 1) len t
    ktuples' _ _ _ = undefined

diagonal :: Ix -> Ix -> DiagIx
diagonal i j = i - j

-- TODO:: Check the limits here...
indexes :: DiagIx -> Int -> Int -> [(Ix, Ix)]
indexes d m n = [(x, x + d) | x <- [d' .. max m n], x < m + n - 1]
  where
    d' = max 0 (-d)

diagonals :: Int -> String -> String -> DiagMap
diagonals k s t = dotplot ls lt
  where
    ls = ktupleMap k s
    lt = ktupleMap k t
    dotplot :: IndexMap -> IndexMap -> DiagMap
    dotplot = Map.intersectionWith (\x y -> [diagonal i j | i <- x, j <- y])

-- Step 2 scoring method for selecting significant diagonals
diagonalScores :: Int -> String -> String -> [(DiagIx, Score)]
diagonalScores k s t = bestscores
  where
    diags :: DiagMap
    diags = diagonals k s t

    scores :: [(DiagIx, Score)]
    scores = count $ concat diags
    -- or count $ foldr (++) [] diags ?

    --TODO:: threshold should be within 2 std devs of mean
    discardOutliers :: [(DiagIx, Score)] -> [(DiagIx, Score)]
    discardOutliers = id

    bestscores :: [(DiagIx, Score)]
    bestscores =
      take 10 $ discardOutliers $ sortBy (\(_, x) (_, y) -> compare y x) scores

-- Step 3
-- Eg, diagonalsRescored 3 s1 t1

diagonalsRescored :: Int -> String -> String -> [(DiagIx, Score)]
diagonalsRescored k s t =
  map
    ((\(d, (_, runLengths, gapCount)) ->
        (d, matchScore * sum (filter (>= k) runLengths) + gapScore * gapCount)) .
     diagonalRescored)
    diagCandidates
--diagonalsRescored k s t = map diagonalRescored diagCandidates
  where
    diagCandidates :: [DiagIx]
    diagCandidates = map fst $ diagonalScores k s t

    diagonalRescored :: DiagIx -> (DiagIx, Tally)
    diagonalRescored d
      | d >= 0 = (d, diagonalRescored' (drop d s) t)
      | otherwise = (d, diagonalRescored' s (drop (-d) t))

    diagonalRescored' :: String -> String -> Tally
    diagonalRescored' s' t' = diagonalRescored'' s' t' (0, [], 0)
    diagonalRescored'' :: String -> String -> Tally -> Tally
    diagonalRescored'' [] _ (runLength, runLengths, gapCount) =
      (0, runLength : runLengths, gapCount)

    diagonalRescored'' _ [] (runLength, runLengths, gapCount) =
      (0, runLength : runLengths, gapCount)
    diagonalRescored'' (x:xs) (y:ys) (runLength, runLengths, gapCount)
      | x == y && runLength == k - 1 =
        match xs ys (runLength + 1, runLengths, gapCount)
      | x == y = diagonalRescored'' xs ys (runLength + 1, runLengths, gapCount)
      | otherwise = diagonalRescored'' xs ys (0, runLengths, gapCount)
      where
        match :: String -> String -> Tally -> Tally
        match [] _ (rl, rls, gc) = (0, rl : rls, gc)
        match _ [] (rl, rls, gc) = (0, rl : rls, gc)
        match (a:as) (b:bs) (rl, rls, gc)
          | a == b = match as bs (rl + 1, rls, gc)
          | otherwise = gap as bs (0, rl : rls, gc + 1)

        gap :: String -> String -> Tally -> Tally
        gap [] _ tally = tally
        gap _ [] tally = tally
        gap (a:as) (b:bs) (rl, rls, gc)
          | a /= b = gap as bs (rl, rls, gc + 1)
          | otherwise = match as bs (1, rls, gc)
