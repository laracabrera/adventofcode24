module Day02 where

import Paths_aoc2024 (getDataFileName)
import Data.List.Split

evaluateLine :: [Int] -> Bool
evaluateLine l = all (\i -> i >= -3 && i <= -1) ls || all (\i -> i >= 1 && i <= 3) ls
                 where ls = zipWith (-) (tail l) l

removingOne :: [Int] -> [[Int]]
removingOne xs = [take i xs ++ drop (i+1) xs | i <- [0..length xs-1]]

evaluateBoth :: [Int] -> Bool
evaluateBoth xs = evaluateLine xs || any evaluateLine (removingOne xs)

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  let inputNums = map (splitOn " ") inputLines                -- inputNums = [["1", "4", "3", ...], ["2", "7"]]
      inputInts = map (map (\x -> read x :: Int)) inputNums   -- inputInts = [[1, 4, 3, ...], [2, 7]]
      valids = map evaluateBoth inputInts
      total = length (filter id valids)
  print total