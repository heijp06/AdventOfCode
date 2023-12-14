module Lib
    ( parse
    , part1
    , part2
    ) where

import qualified Data.Set as Set

type Position = (Int, Int)
type Rocks = Set.Set Position
type Mirrors = Set.Set Position
type Platform = (Rocks, Mirrors)

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> Platform
parse xs = foldl combinePlatform (Set.empty, Set.empty) $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs

combinePlatform :: Platform -> (Position, Char) -> Platform
combinePlatform platform (_, '.') = platform
combinePlatform (rocks, mirrors) (pos, '#') = (Set.insert pos rocks, mirrors)
combinePlatform (rocks, mirrors) (pos, 'O') = (rocks, Set.insert pos mirrors)
combinePlatform _ something = error $ "Failed to parse " ++  show something