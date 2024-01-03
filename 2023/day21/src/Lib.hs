module Lib
    ( display
    , getAB
    , parse
    , part1
    , part2
    , solve
    , step
    ) where

import qualified Data.Set as Set

type Position = (Int, Int)

type Grid = Set.Set Position

part1 :: [String] -> Int
part1 xss = Set.size $ solve 64 (Set.singleton start) grid
    where
        (start, grid) = parse xss

solve :: Int -> Set.Set Position -> Grid -> Set.Set Position
solve n start grid = last . take (n + 1) $ iterate (step grid) start

display :: Int -> [String] -> [String]
display n xs
    = [ [ if pos `Set.member` grid then '#' else if pos `Set.member` positions then 'O' else '.'
        | column <- [0..width-1]
        , let pos = (row, column)
        ]
      | row <- [0..height-1]
      ]
    where
        (start, grid) = parse xs
        positions = solve n (Set.singleton start) grid
        height = length xs
        width = length $ head xs

part2 :: [String] -> Int
part2 xs = undefined
    where
        steps = 26501365
        (a, b') = getAB 201 xs
        (a', b) = getAB 200 xs

getAB :: Int -> [String] -> (Int, Int)
getAB n xs = (a, b)
    where
        (start, grid) = parse xs
        positions = solve n (Set.singleton start) grid
        a = Set.size $ Set.filter (\ (row, column) -> abs (row - 65) + abs (column - 65) <= 65) positions
        b = Set.size (Set.filter (\ (row, column) -> row >= 0 && row <= 130 && column >= 0 && column <= 130) positions) - a

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

