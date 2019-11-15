-- https://www.hackerrank.com/challenges/functions-or-not/problem
--
-- You are given a set of unique (x,y) ordered pairs constituting a relation.
-- The x-values form the domain, and the y-values form the range to which they
-- map. For each of these relations, identify whether they may possibly
-- represent a valid function or not.

module FunctionOrNot where

import Control.Monad
import Data.List (find)

chkFn :: [(Integer, Integer)] -> String
chkFn pairs = if g pairs then "YES" else "NO"
  where
    g (x:[]) = True
    g ((dom, rng):xs) =
      case find (\(dom2, rng2) -> dom == dom2 && rng /= rng2) xs of
        Nothing -> g xs
        Just _ -> False

main :: IO ()
main = do
  n <- readLn :: IO Int

  forM_ [1..n] $ \_ -> do
    m <- readLn :: IO Int
    inputs <- replicateM m getLine
    putStrLn $ chkFn $ map ((\[x1,x2] -> (x1,x2)) . map read . words) inputs



