module Lib
    ( part1
    , part2
    , partitions
    ) where

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

partitions :: Int -> Int -> [[Int]]
partitions n _ | n <= 0 = []
partitions _ len | len <= 0 = []
partitions n 1 = [[n]]
partitions n len = [ n - i : p | i <- [ 1 .. n - 1 ], p <- partitions i (len - 1) ]