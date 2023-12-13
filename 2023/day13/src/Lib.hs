module Lib
    ( Position
    , parse
    , part1
    , part2
    , reflectHorizontal
    , reflectVertical
    , replace
    , smudge
    ) where

import Data.List (inits, tails, transpose)
import Text.Printf (printf)

import Debug.Trace (trace)

type Position = (Int, Int)
type Valley = [String]

part1 :: [String] -> Int
part1 = sum . map reflect . parse

part2 :: [String] -> Int
part2 = sum . map smudge . parse

reflect :: Valley -> Int
reflect xs = 100 * reflectHorizontal xs + reflectVertical xs

reflectHorizontal :: Valley -> Int
reflectHorizontal xs = case (top, bottom) of
                        ([], []) -> 0
                        (_, []) -> length (head top) `div` 2
                        ([], _) -> length xs - length (head bottom) `div` 2
                        _ -> error $ printf "Reflection from both top and bottom, top = %s, bottom = %s." (show top) (show bottom)
    where
        top = [ s | s <- inits xs, not $ null s, even $ length s, s == reverse s ]
        bottom = [ s | s <- tails xs, not $ null s, even $ length s, s == reverse s ]

reflectVertical :: Valley -> Int
reflectVertical = reflectHorizontal . transpose

parse :: [String] -> [Valley]
parse = foldr combineParse [[]]

smudge :: Valley -> Int
smudge valley = (50*) . sum . filter (/=except) $ map (reflectHorizontal . replace valley) [ (row, column) | row <- [0..height-1], column <- [0..width-1] ]
-- smudge valley = (50*) $ sum . map ((\i -> trace (show i) i) . reflectHorizontal . replace valley) [ (row, column) | row <- [0..height-1], column <- [0..width-1] ]
    where
        height = length valley
        width = length (head valley)
        except = reflectHorizontal valley

combineParse :: String -> [Valley] -> [Valley]
combineParse "" acc = [] : acc
combineParse x (ys:acc) = (x:ys) : acc
combineParse x [] = error $ "acc is empty. x = " ++ x

replace :: Valley -> Position -> Valley
replace xs (row, column) = take row xs ++ [newLine] ++ drop (row + 1) xs
    where
        line = xs !! row
        char = line !! column
        newChar = if char == '.' then '#' else '.'
        newLine = take column line ++ [newChar] ++ drop (column + 1) line