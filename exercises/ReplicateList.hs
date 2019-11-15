-- https://www.hackerrank.com/challenges/fp-list-replication/problem
--
-- Given a list, repeat each element in the list n amount of times.

module ReplicateList where

f :: Int -> [Int] -> [Int]
f n xs = (++) (replicate n $ head xs) (f n $ tail xs)
f n _ = []

f2 :: Int -> [Int] -> [Int]
f2 n xs = concatMap (\num -> replicate n num) xs

f3 :: Int -> [Int] -> [Int]
f3 n xs = (concatMap . replicate) n xs

f4 :: Int -> [Int] -> [Int]
f4 n xs = [num | num <- xs, _ <- [1..n]]
