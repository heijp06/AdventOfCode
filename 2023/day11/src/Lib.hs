module Lib
    ( expand
    , parse
    , part1
    , part2
    ) where

import Data.List (sort, tails)
import Data.Tuple (swap)

type Position = (Int, Int)

part1 :: [String] -> Int
part1 = solve 2

part2 :: [String] -> Int
part2 = solve 1000000

solve :: Int -> [String] -> Int
solve n xs = foldr add 0 gss
    where
        positions = expand n $ parse xs
        gss = init $ tails positions

add :: [Position] -> Int -> Int
add [] _ = error "Empty list of positions"
add (pos:ps) acc = acc + foldr (manhattan pos) 0 ps

manhattan :: Position -> Position -> Int -> Int
manhattan (x1, y1) (x2, y2) acc = acc + abs (x2 - x1) + abs (y2 - y1)

parse :: [String] -> [Position]
parse xs = [ pos | (pos, c) <- zip ((,) <$> [0..height-1] <*> [0..width-1]) $ concat xs, c == '#' ]
    where
        height = length xs
        width = length $ head xs

expand :: Int -> [Position] -> [Position]
expand n positions = sort $ map swap expanded
    where
        (_, _, expandedHeight) = foldl (expand' n) (0, 0, []) positions
        (_, _, expanded) = foldl (expand' n) (0, 0, []) . sort $ map swap expandedHeight

expand' :: Int -> (Int, Int, [Position]) -> Position -> (Int, Int, [Position])
expand' _ (_, _, []) pos@(x, _) = (0, x, [pos])
expand' n (extra, x', ps) (x, y) = (newExtra, x, ps ++ [(x + newExtra, y)])
    where
        newExtra = extra + max 0 ((x - x' - 1) * (n - 1))
