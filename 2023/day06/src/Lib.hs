module Lib
    ( count
    , numbers
    , parse
    , part1
    , part2
    ) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 = product . map (uncurry count) . parse

part2 :: [String] -> Int
part2 xs = count (largeNumber $ head xs) (largeNumber $ xs !! 1)

parse :: [String] -> [(Int, Int)]
parse xs = zip (numbers $ head xs) $ numbers (xs !! 1)

numbers :: String -> [Int]
numbers xs = [ read x | x <- tail $ splitOn " " xs, not $ null x ]

largeNumber :: String -> Int
largeNumber = read . filter isDigit

count :: Int -> Int -> Int
count t d = t1 - t0 + 1 - delta
    where
        disc = t * t - 4 * d
        root = (sqrt $ fromIntegral disc) :: Double
        t0 = ceiling $ (fromIntegral t - root) / 2
        t1 = floor $ (fromIntegral t + root) / 2
        delta = if (floor root :: Int) == ceiling root then 2 else 0
