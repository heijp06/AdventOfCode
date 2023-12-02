module Lib
    ( part1
    , part2
    ) where

import Game

part1 :: [String] -> Int
part1 xs = sum [ identifier game | game <- map parse xs, all possible (draws game) ]

part2 :: [String] -> Int
part2 = undefined

possible :: Draw -> Bool
possible = undefined