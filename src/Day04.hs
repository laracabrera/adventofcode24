module Day04 where

import Paths_aoc2024 (getDataFileName)

import Text.Regex.PCRE ((=~))
import Data.Matrix

pattern :: String
pattern = "(XMAS|SAMX)"

findInLines :: [String] -> Int
findInLines = sum . map (=~ pattern)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)
  let m = fromLists inputLines
      columns = map (toList . (`getCol` m)) [0..ncols m-1]
  print $ findInLines inputLines + findInLines columns + findInLines diags
