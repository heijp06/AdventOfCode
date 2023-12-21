module Lib
    ( parse
    , part1
    , part2
    ) where

import qualified Data.Set as Set

type Position = (Int, Int)

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> (Position, Set.Set Position)
parse xss = (start, grid)
    where
        height = length xss
        width = length $ head xss
        grid = Set.fromList [ fst x | x <- zip ((,) <$> [0..height-1] <*> [0..width-1]) (concat xss), snd x == '#' ] 
        start = head [ fst x | x <- zip ((,) <$> [0..height-1] <*> [0..width-1]) (concat xss), snd x == 'S' ] 

