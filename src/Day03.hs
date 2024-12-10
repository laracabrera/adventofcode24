{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Day03 where

import Paths_aoc2024 (getDataFileName)
import Text.Regex.PCRE ((=~))

-- Part 1 regexp: mul\((\d{1,3}),(\d{1,3})\)

pattern :: String
pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)"

day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  let matches = head inputLines =~ pattern :: [[String]]                               -- matches = [["mul(2,4)","2","4"],["mul(5,5)","5","5"],["mul(11,8)","11","8"],["mul(8,5)","8","5"]]
      total = sum (map (\(_:a:b:_) -> (read a :: Int) * (read b :: Int)) matches)
  print total

