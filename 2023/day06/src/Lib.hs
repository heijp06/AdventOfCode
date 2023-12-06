module Lib
    ( count
    , numbers
    , parse
    , part1
    , part2
    ) where

import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> [(Int, Int)]
parse xs = zip (numbers $ head xs) $ numbers (xs !! 1)

numbers :: String -> [Int]
numbers xs = [ read x | x <- tail $ splitOn " " xs, not $ null x ]

count :: Int -> Int -> Int
count = undefined
