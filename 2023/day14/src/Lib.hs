module Lib
    ( parse
    , part1
    , part2
    ) where

type Position = (Int, Int)
type Rocks = [Position]
type Mirrors = [Position]
type Platform = (Rocks, Mirrors)

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> Platform
parse xs = foldl combinePlatform ([], []) $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs

combinePlatform :: Platform -> (Position, Char) -> Platform
combinePlatform platform (_, '.') = platform
combinePlatform (rocks, mirrors) (pos, '#') = (pos : rocks, mirrors)
combinePlatform (rocks, mirrors) (pos, 'O') = (rocks, pos : mirrors)
combinePlatform _ something = error $ "Failed to parse " ++  show something