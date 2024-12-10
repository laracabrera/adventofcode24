{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day03 where

import Paths_aoc2024 (getDataFileName)
import Text.Regex.PCRE ((=~))

pattern :: String
pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)"

process :: [[String]] -> Bool -> Int -> Int
process [] _ ac = ac
process (x:xs) enabled ac = case x of
  ("don't()":_)   -> process xs False ac
  ("do()":_)      -> process xs True ac
  (_:a:b:_)       -> (if enabled then process xs True (ac + ((read a :: Int) * (read b :: Int))) else process xs False ac)
-- process (("don't()":_)):xs        = process xs False ac
--   | ("do()":_):xs           = process xs True ac
--   | (_:a:b:_):xs True ac    = process xs True (ac + ((read a :: Int) * (read b :: Int)))
--   | (_:a:b:_):xs False ac   = process xs False ac

day03 :: IO ()
day03 = do
  inputLines <- getDataFileName "day03-input.txt" >>= readFile
  let matches = inputLines =~ pattern :: [[String]]                               -- matches = [["mul(2,4)","2","4"],["mul(5,5)","5","5"],["mul(11,8)","11","8"],["mul(8,5)","8","5"]]
      total = process matches True 0
  print total

