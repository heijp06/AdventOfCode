module Lib
    ( parse
    , part1
    , part2
    , tiltNorth
    ) where

import Data.List (sort)

type Position = (Int, Int)
type Rocks = [Position]
type Mirrors = [Position]
type Platform = (Rocks, Mirrors)

part1 :: [String] -> Int
part1 xs = sum . map ((height-) . fst) . snd . tiltNorth $ parse xs
    where
        height = length xs

part2 :: [String] -> Int
part2 = undefined

tiltNorth :: Platform -> Platform
tiltNorth (rocks, mirrors) = foldl combineTilt (rocks, []) $ sort mirrors

combineTilt :: Platform -> Position -> Platform
combineTilt (rocks, mirrors) (row, col) = (rocks, (newRow, col) : mirrors)
    where
        newRow = 1 + maximum (-1 : [ r | (r, c) <- rocks ++ mirrors, col == c && r < row ])

parse :: [String] -> Platform
parse xs = foldr combinePlatform ([], []) $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs


combinePlatform :: (Position, Char) -> Platform -> Platform
combinePlatform (_, '.') platform = platform
combinePlatform (pos, '#') (rocks, mirrors) = (pos : rocks, mirrors)
combinePlatform (pos, 'O') (rocks, mirrors) = (rocks, pos : mirrors)
combinePlatform something _ = error $ "Failed to parse " ++  show something