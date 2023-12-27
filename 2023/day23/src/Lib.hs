{-# LANGUAGE RecordWildCards #-}

module Lib
    ( parse
    , part1
    , part2
    ) where

import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set
    
type Position = (Int, Int)
type Direction = (Int, Int)
type Grid = Array.Array Position Char
type Positions = Map.Map Position PathState

data PathState = PathState { steps :: Int
                           , position :: Position
                           , direction :: Direction
                           }

data PathState2 = PathState2 { steps2 :: Int
                             , seen :: Set.Set Position
                             , current :: Position
                             }

part1 :: [String] -> Int
part1 = solve . parse

part2 :: [String] -> Int
part2 = solve2 . parse

solve2 :: Grid -> Int
solve2 grid = fst
            . head
            . dropWhile (not . null . snd)
            $ iterate (step2 grid end) (0, [PathState2 0 (Set.singleton start) start])
    where
        start = fst . head . filter ((=='.') . snd) $ Array.assocs grid
        end = fst . last . filter ((=='.') . snd) $ Array.assocs grid

step2 :: Grid -> Position -> (Int, [PathState2]) -> (Int, [PathState2])
step2 grid end (highest, states) = foldr (combineStep2 grid end) (highest, []) states

combineStep2 :: Grid -> Position -> PathState2 -> (Int, [PathState2]) -> (Int, [PathState2])
combineStep2 grid end state2 (highest, states) = (max newSteps2 highest, statesToAdd ++ states)
    where
        newStates = [ PathState2 (steps2 state2 + 1) (Set.insert newPosition (seen state2)) newPosition
                    | direction <- [(1, 0), (0, 1), (-1, 0), (0, -1)]
                    , let newPosition = current state2 `add` direction
                    , newPosition `Set.notMember` seen state2
                    , Array.inRange (Array.bounds grid) newPosition 
                    , grid ! newPosition /= '#'
                    ]
        newSteps2 = maximum $ 0 : [ steps2 s | s <- newStates, current s == end ]
        statesToAdd = filter ((/=end) . current) newStates

solve :: Grid -> Int
solve grid = steps
           . (Map.! end)
           . fst
           . head
           . dropWhile (not . Map.null . snd)
           $ iterate (step grid) (Map.empty, Map.singleton start $ PathState 0 (1, 0) start)
    where
        start = fst . head . filter ((=='.') . snd) $ Array.assocs grid
        end = fst . last . filter ((=='.') . snd) $ Array.assocs grid

step :: Grid -> (Positions, Positions) -> (Positions, Positions)
step grid (positions, current) = foldr (combineStep grid) (both, Map.empty) current
    where
        both = Map.unionWith maxSteps positions current

combineStep :: Grid -> PathState -> (Positions, Positions) -> (Positions, Positions)
combineStep grid PathState{..} (positions, current) = (positions, Map.unionWith maxSteps current newPositions)
    where
        newPositions = Map.fromList
                     [ (newPosition, PathState (steps + 1) newPosition newDirection)
                     | (newDirection, symbol) <- [ ((1, 0), 'v'), ((0, 1), '>'), ((-1, 0), '^'), ((0, -1), '<') ]
                     , let newPosition = add position newDirection
                     , newDirection /= neg direction
                     , Array.inRange (Array.bounds grid) newPosition 
                     , grid ! newPosition `elem` [ '.', symbol ]
                     ]

maxSteps :: PathState -> PathState -> PathState
maxSteps state1 state2 = if steps state2 < steps state1 then state1 else state2

add :: Position -> Direction -> Position
add (row, column) (dRow, dColumn) = (row + dRow, column + dColumn)

neg :: Direction -> Direction
neg (dRow, dColumn) = (-dRow, -dColumn)

parse :: [String] -> Grid
parse xs = Array.listArray ((0, 0), (height - 1, width - 1)) $ concat xs
    where
        height = length xs
        width = length $ head xs