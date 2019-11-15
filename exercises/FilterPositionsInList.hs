-- https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list/problem
--
-- For a given list with N integers, return a new list removing the elements at
-- odd positions.

module FilterPositionsInList where

f (_:x:xs) = x : f xs
f _ = []

f2 (_:xs) = (head xs) : f (tail xs)
f2 _ = []

f3 :: [Int] -> [Int]
f3 lst = map fst $ filter (even . snd) $ zip lst [1..]

f4 lst = [a | (a,b) <- zip lst [1..], even b]
