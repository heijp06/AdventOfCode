module Lib
    ( part1
    , part2
    ) where

import Control.Applicative (liftA2)

part1 :: [String] -> Int
part1 = solve last sum

part2 :: [String] -> Int
part2 = solve head $ foldr1 (flip subtract)

solve :: ([Int] -> Int) -> ([Int] -> Int) -> [String] -> Int
solve item combine = sum
                   . map
                   ( combine
                   . map item
                   . takeWhile (any (/=0))
                   . iterate (liftA2 (zipWith subtract) id tail)
                   . map read
                   . words
                   )