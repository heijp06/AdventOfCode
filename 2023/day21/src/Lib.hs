module Lib
    ( parse
    , part1
    , part2
    , solve
    , step
    ) where

import qualified Data.Set as Set

type Position = (Int, Int)

type Grid = Set.Set Position

part1 :: [String] -> Int
part1 = solve 64

solve :: Int -> [String] -> Int
solve n xss = Set.size . last . take (n + 1) $ iterate (step grid) (Set.singleton start)
    where
        (start, grid) = parse xss

part2 :: [String] -> Int
part2 = undefined

step :: Grid -> Grid -> Grid
step grid current = new
    where
        new = Set.fromList [ (x + dx, y + dy) | (x, y) <- Set.toList current
                                              , (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
                                              , (x + dx, y + dy) `Set.notMember` grid
                                              ]

parse :: [String] -> (Position, Set.Set Position)
parse xss = (start, grid)
    where
        height = length xss
        width = length $ head xss
        grid = Set.fromList [ fst x | x <- zip ((,) <$> [0..height-1] <*> [0..width-1]) (concat xss), snd x == '#' ] 
        start = head [ fst x | x <- zip ((,) <$> [0..height-1] <*> [0..width-1]) (concat xss), snd x == 'S' ] 

