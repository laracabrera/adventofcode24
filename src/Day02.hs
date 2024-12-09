module Day02 where

import Paths_aoc2024 (getDataFileName)
import Data.List.Split

evaluateLine :: [Int] -> Bool
evaluateLine l = all (\i -> i >= -3 && i <= -1) l || all (\i -> i >= 1 && i <= 3) l

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  let inputNums = map (splitOn " ") inputLines                -- inputNums = [["1", "4", "3", ...], ["2", "7"]]
      inputInts = map (map (\x -> read x :: Int)) inputNums   -- inputInts = [[1, 4, 3, ...], [2, 7]]
      diffs = map (\x -> zipWith (-) (tail x) x) inputInts
      valids = map evaluateLine diffs
      total = length (filter id valids)
  print total