module Lib
    ( expand
    , parse
    , part1
    , part2
    ) where

import Data.Tuple (swap)

type Position = (Int, Int)

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> [Position]
parse xs = [ pos | (pos, c) <- (zip ((,) <$> [0..height-1] <*> [0..width-1]) $ concat xs), c == '#' ]
    where
        height = length xs
        width = length $ head xs

expand :: [Position] -> [Position]
expand positions = map swap expanded
    where
        (_, _, expandedHeight) = foldl expand' (0, 0, []) positions
        (_, _, expanded) = foldl expand' (0, 0, []) $ map swap expandedHeight

expand' :: (Int, Int, [Position]) -> Position -> (Int, Int, Position)
expand' = undefined
