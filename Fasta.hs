module Fasta where

import           Control.Monad
import Data.List
import           Data.List.Unique
import qualified Data.Map         as Map

-- http://users.csc.calpoly.edu/~dekhtyar/448-Spring2013/lectures/lec08.448.pdf

type Ix = Int -- String index

type DiagIx = Int -- Diagonal index

type Score = Int

type IndexList = [(String, [Ix])]

type IndexMap = Map.Map String [Ix]

type DiagMap = Map.Map String [DiagIx]

type DiagonalScores = Map.Map Int Int

type Subst = Bool -> Score

sigma :: String
sigma = "ATCG"

s1, t1 :: String
s1 = "ATCGTATCG"

t1 = "CAGATCGTCTCGAT"

score :: Bool -> Int
score True = 5
score False = -4

els :: Int -> String -> [String]
els = replicateM

ktupleMap :: Int -> String -> IndexMap
ktupleMap k s = Map.fromListWith (++) $ ktuples k s

ktuples :: Int -> String -> [(String, [Ix])]
ktuples k s
  | k < lens = ktuples' k 0 lens s
  | otherwise = undefined
  where
    lens = length s

    -- TODO:: think about whether this is better done with a traverse
    ktuples' :: Int -> Int -> Int -> String -> IndexList
    ktuples' k ix len s@(_:t) =
      if ix + k > len
        then []
        else (take k s, [ix]) : ktuples' k (ix + 1) len t
    ktuples' _ _ _ _ = undefined

diagonal :: Ix -> Ix -> DiagIx
diagonal i j = i - j

-- TODO:: Check the limits here...
indexes :: DiagIx -> Int -> Int -> [(Ix, Ix)]
indexes d m n = [(x, x + d) | x <- [d' .. max m n], x < m + n -1]
    where d' = max 0 (-d)

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
    
    fullscores :: [(DiagIx, Score)]
    fullscores = count $ concat diags
    -- or count $ foldr (++) [] diags ?
    
    scores :: [Int]
    scores = map snd fullscores
    
    --TODO:: threshold should be within 2 std devs of mean
    discardOutliers :: [(DiagIx, Score)] -> [(DiagIx, Score)]
    discardOutliers = id
    
    bestscores :: [(DiagIx, Score)]
    bestscores = take 10 $ discardOutliers $ sortBy (\(_, x) (_, y) -> compare y x) fullscores


-- Step 3
diagonalsRescored :: Int -> String -> String -> Subst -> [(DiagIx, Score)]
--diagonalsRescored k s t subst = map (\(dx, dy) -> subst (dx == dy)) diagIndexes
diagonalsRescored k s t subst = map diagScore diagCandidates
  where
    lens = length s
    lent = length t
    
    diagCandidates :: [DiagIx]
    diagCandidates = map fst $ diagonalScores k s t
    
    
--    diagIndex :: DiagIx -> (DiagIx, [(Ix, Ix)])
--    diagIndex d = (d, indexes d lens lent)
--    
--    diagIndexes :: [(DiagIx, [(Ix, Ix)])]
--    diagIndexes = map diagIndex diagCandidates
   
    diagScore :: DiagIx -> (DiagIx, Score)
    diagScore d = (d, sum $ map (\(dx, dy) -> score (dx == dy)) (indexes d lens lent))
    