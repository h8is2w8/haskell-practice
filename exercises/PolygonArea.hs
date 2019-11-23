-- https://www.hackerrank.com/challenges/lambda-march-compute-the-perimeter-of-a-polygon/problem

-- You are given the cartesian coordinates of a set of points in a 2D plane.
-- Compute the area of polygon P


-- https://www.mathsisfun.com/geometry/area-irregular-polygons.html
-- https://iq.opengenus.org/area-of-polygon-shoelace/
-- https://www.101computing.net/the-shoelace-algorithm/


module PolygonArea where

import Control.Monad

-- calcs polygon area
area :: Fractional t => [(t, t)] -> t
area points = abs $ go points 0
  where
    fstPoint = head points
    go (p:[]) acc = lineArea p fstPoint + acc
    go (p1:p2:rest) acc = go (p2 : rest) (lineArea p1 p2 + acc)


-- calcs area under a line
lineArea :: Fractional a => (a, a) -> (a, a) -> a
lineArea (x1, y1) (x2, y2) =
  (y1 + y2) * (x2 - x1) / 2


points = [
  (0.72, 2.28),
  (2.66, 4.71),
  (5.0,  3.5),
  (3.63, 2.52),
  (4.0,  1.6),
  (1.9,  1.0)
  ]

-- 4
-- 0 0
-- 0 1
-- 1 1
-- 1 0
-- => 1
main :: IO ()
main = do
  n <- readLn :: IO Int
  inputs <- replicateM n getLine
  putStrLn $ show $ area $ map ((\[x,y] -> (x,y)) . map read . words) inputs
