module Lib
    ( part1
    , part2
    ) where

import Control.Applicative (liftA2)

part1 :: [String] -> Int
part1 = sum . map row

row :: String -> Int
row = sum
    . map last
    . takeWhile (not . null)
    . iterate (liftA2 (zipWith subtract) id tail)
    . map read
    . words

part2 :: [String] -> Int
part2 = undefined
