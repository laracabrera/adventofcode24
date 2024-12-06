module Day01 where

import Paths_aoc2024 (getDataFileName)
import Data.List.Split
-- Part 1:
-- import Data.List

procesarLinea :: String -> (Int, Int)
procesarLinea l = let nums = splitOn "   " l in
            (read (head nums) :: Int, read (nums !! 1) :: Int)

conteo :: [Int] -> [Int] -> Int -> Int
conteo xss ys ac = case xss of
    []      -> ac        -- Caso base
    (x:xs)  -> conteo xs ys (length (filter (== x) ys) * x) + ac

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  let pares = map procesarLinea inputLines
      a = map fst pares
      b = map snd pares
      -- Part 1:
      -- c = zipWith (-) (sort b) (sort a)
  -- print $ sum $ map abs c
  print $ conteo a b 0 -- Part 2
