-- https://www.hackerrank.com/challenges/fp-reverse-a-list/problem
--
-- You are given a list of elements. Reverse the list without using the reverse
-- function.

module ReverseList where

rev :: [a] -> [a]
rev (x:xs) = (rev xs) ++ [x]
rev _ = []
