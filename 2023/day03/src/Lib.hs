{-# LANGUAGE RecordWildCards #-}

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

import Data.Char (digitToInt, isDigit)
import Data.Map ((!))
import qualified Data.Map as Map

type Position = (Int, Int)
type Grid = Map.Map Position Char

data Number = Number { value :: Int
                     , start :: Position
                     , end :: Position
                     } deriving Show

data Acc = Acc { previousPos :: Position
               , factor :: Int
               , numbers :: [Number]
               }

part1 :: [String] -> Int
part1 = sum . map value . solve (not . isDigit)

part2 :: [String] -> Int
part2 xss = foldr (add2 connectedToStar) 0 starPositions
    where
        connectedToStar = solve isStar xss
        starPositions = Map.keys . symbols isStar $ buildGrid xss
        isStar = (=='*')

add2 :: [Number] -> Position -> Int -> Int
add2 numbers (starX, starY) acc = acc + gearRatio
    where
        connected = [ value number | number <- numbers, isConnected number ]
        isConnected Number{..} = abs (starY - snd start) <= 1 && starX >= fst start - 1 && starX <= fst end + 1
        gearRatio = if length connected == 2 then product connected else 0

solve :: (Char -> Bool) -> [String] -> [Number]
solve isSymbol xss = numbers $ foldr (add connected) (Acc (0, 0) 0 []) connectedCoordinates 
    where
        connected = allConnected isSymbol $ buildGrid xss
        connectedCoordinates = filter (`Map.member` connected) $ coordinates width height
        width = getWidth xss
        height = getHeight xss

add :: Grid -> Position -> Acc -> Acc
add connected pos@(x, y) Acc{..} = if isDigit char
                                        then Acc { previousPos = pos
                                                 , factor = newFactor
                                                 , numbers = if newFactor == 1
                                                                then Number (digitToInt char) pos pos : numbers
                                                                else let number = head numbers
                                                                     in Number { value = value number + (digitToInt char) * newFactor
                                                                               , start = pos
                                                                               , end = end number
                                                                               } : tail numbers
                                                 }
                                        else Acc pos 0 numbers
    where
        (prevX, prevY) = previousPos
        char = connected ! pos
        newFactor
            | factor == 0 = 1
            | y == prevY && x == prevX - 1 = 10 * factor
            | otherwise = 1

allConnected :: (Char -> Bool) -> Grid -> Grid
allConnected isSymbol grid = fst . head . dropWhile (not . Map.null . snd) $ iterate doStep (Map.empty, start)
    where
        start = symbols isSymbol grid
        doStep (connected, extra) = let newConnected = Map.union connected extra in (newConnected, step newConnected grid)

step :: Grid -> Grid -> Grid
step connected grid = Map.filterWithKey isAdjacent notConnected
    where
        notConnected = Map.difference grid connected
        isAdjacent k _ = or [ Map.member pos connected | pos <- neighbours k ]
        neighbours (x, y) = [ (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

symbols :: (Char -> Bool) -> Grid -> Grid
symbols = Map.filter

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