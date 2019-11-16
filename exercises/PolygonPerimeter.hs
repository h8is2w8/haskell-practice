-- https://www.hackerrank.com/challenges/lambda-march-compute-the-perimeter-of-a-polygon/problem

-- You are given the cartesian coordinates of a set of points in a 2D plane.
-- Compute the perimeter of polygon P

module PolygonPerimeter where

import Control.Monad

perimeter :: Floating t => [(Int, Int)] -> t
perimeter points = go points 0
  where
    fstPoint = head points
    go (p:[]) acc = dist p fstPoint + acc
    go (p:rest) acc = go rest (dist p (head rest) + acc)

dist :: Floating a => (Int, Int) -> (Int, Int) -> a
dist (x1, y1) (x2, y2) =
  sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


points = [(0, 0), (0, 2), (2, 2), (2, 0)] :: [(Int, Int)]

-- 3
-- 1043 770
-- 551 990
-- 681 463

main :: IO ()
main = do
  n <- readLn :: IO Int
  inputs <- replicateM n getLine
  putStrLn $ show $ perimeter $ map ((\[x,y] -> (x,y)) . map (read::String->Int) . words) inputs
