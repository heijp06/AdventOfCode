module Lib
    ( parse
    , part1
    , part2
    ) where

import qualified Data.Set as Set

type Position = (Int, Int)
type Direction = (Int, Int)

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> [(Direction, Int)]
parse = map parseLine

outline :: [(Direction, Int)] -> Set.Set Position
outline = snd . foldl combineOutline ((0, 0), Set.singleton (0, 0))

combineOutline :: (Position, Set.Set Position) -> (Direction, Int) -> (Position, Set.Set Position)
combineOutline = undefined

move :: Position -> Direction -> Position
move (row, column) (dRow, dColumn) = (row + dRow, column + dColumn)

mul :: Direction -> Int -> Direction
mul (dRow, dColumn) n = (n * dRow, n * dColumn)
        
parseLine :: String -> (Direction, Int)
parseLine xs = case words xs of
                [d, l, _] -> (direction d, read l)
                _ -> error $ "Cannot parse " ++ xs

direction :: String -> Direction
direction "U" = (-1, 0)
direction "D" = (1, 0)
direction "L" = (0, -1)
direction "R" = (0, 1)
direction xs = error $ "Unknown direction " ++ xs
        
