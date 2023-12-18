module Lib
    ( fill
    , outline
    , parse
    , part1
    , part2
    , rows
    ) where

import Data.Ix (range)
import qualified Data.Set as Set

type Position = (Int, Int)
type Direction = (Int, Int)
data ScanState = Out | In | OutBorderUp | OutBorderDown | InBorderUp | InBorderDown

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> [(Direction, Int)]
parse = map parseLine

rows :: [String] -> [String]
rows xs = [ [ if (row, column) `Set.member` positions then '#' else ' ' | column <- [minColumn..maxColumn] ] | row <- [minRow..maxRow] ]
    where
        positions = outline $ parse xs
        minRow = minimum (Set.map fst positions) - 1
        minColumn = minimum (Set.map snd positions) - 1
        maxRow = maximum (Set.map fst positions) + 1
        maxColumn = maximum (Set.map snd positions) + 1

fill :: Set.Set Position -> Set.Set Position
fill positions = snd . foldl combineFill (Out, positions) $ range ((minRow, minColumn), (maxRow, maxColumn))
    where
        minRow = minimum (Set.map fst positions) - 1
        minColumn = minimum (Set.map snd positions) - 1
        maxRow = maximum (Set.map fst positions) + 1
        maxColumn = maximum (Set.map snd positions) + 1

combineFill :: (ScanState, Set.Set Position) -> Position -> (ScanState, Set.Set Position)
combineFill (Out, positions) position@(row, column) | position `Set.member` positions =
    if (row - 1, column) `Set.member` positions
        then
            if (row + 1, column) `Set.member` positions
                then (In, positions)
                else (OutBorderUp, positions)
        else
            if (row + 1, column) `Set.member` positions
                then (OutBorderDown, positions)
                else error "On border, but no border up or down."
combineFill acc@(Out, _) _ = acc
combineFill _ _ = undefined

outline :: [(Direction, Int)] -> Set.Set Position
outline = snd . foldl combineOutline ((0, 0), Set.singleton (0, 0))

combineOutline :: (Position, Set.Set Position) -> (Direction, Int) -> (Position, Set.Set Position)
combineOutline (position, loop) (dir, n) =
    (move position (n `mul` dir), Set.union loop (Set.fromList [move position (i `mul` dir) | i <- [1..n] ]))

move :: Position -> Direction -> Position
move (row, column) (dRow, dColumn) = (row + dRow, column + dColumn)

mul :: Int -> Direction -> Direction
mul n (dRow, dColumn) = (n * dRow, n * dColumn)
        
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
        
