module Lib
    ( part1
    , part2
    ) where

import Data.Char (isDigit, digitToInt)

part1 :: [String] -> Int
part1 = foldr add 0

part2 :: [String] -> Int
part2 = undefined

add :: String -> Int -> Int
add xs acc = acc + (10 * head digits) + last digits
    where
        digits = [ digitToInt c | c <- xs, isDigit c ]
