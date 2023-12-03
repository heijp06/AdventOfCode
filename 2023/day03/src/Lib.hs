module Lib
    ( Grid
    , allConnected
    , buildGrid
    , coordinates
    , part1
    , part2
    , step
    , symbols
    ) where

import Data.Char (isDigit)
import qualified Data.Map as Map

type Position = (Int, Int)
type Grid = Map.Map Position Char

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

allConnected :: Grid -> Grid
allConnected grid = fst . head . dropWhile (not . Map.null . snd) $ iterate doStep (Map.empty, start)
    where
        start = symbols grid
        doStep (connected, extra) = let newConnected = Map.union connected extra in (newConnected, step newConnected grid)

step :: Grid -> Grid -> Grid
step connected grid = Map.filterWithKey isAdjacent notConnected
    where
        notConnected = Map.difference grid connected
        isAdjacent k _ = or [ Map.member pos connected | pos <- neighbours k ]
        neighbours (x, y) = [ (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

symbols :: Grid -> Grid
symbols = Map.filter (not . isDigit)

buildGrid :: [String] -> Grid
buildGrid xss = Map.fromList [ (pos, char) | (pos, char) <- zip (coordinates width height) (concat xss), char /= '.' ]
    where
        width = getWidth xss
        height = getHeight xss

coordinates :: Int -> Int -> [Position]
coordinates width height = (flip (,)) <$> [0..height-1] <*> [0..width-1]

getHeight :: [String] -> Int
getHeight = length

getWidth :: [String] -> Int
getWidth = length . head