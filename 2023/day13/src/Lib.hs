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

type Position = (Int, Int)
type Valley = [String]

part1 :: [String] -> Int
part1 = sum . concatMap reflect . parse

part2 :: [String] -> Int
part2 = sum . map smudge . parse

reflect :: Valley -> [Int]
reflect xs = map (100*) (reflectHorizontal xs) ++ reflectVertical xs

reflectHorizontal :: Valley -> [Int]
reflectHorizontal xs = top ++ bottom
    where
        top = [ length s `div` 2 | s <- inits xs, not $ null s, even $ length s, s == reverse s ]
        bottom = [ length xs - length s `div` 2 | s <- tails xs, not $ null s, even $ length s, s == reverse s ]

reflectVertical :: Valley -> [Int]
reflectVertical = reflectHorizontal . transpose

parse :: [String] -> [Valley]
parse = foldr combineParse [[]]

smudge :: Valley -> Int
smudge valley = (50*) . sum . filter (/=except) $ concatMap (reflectHorizontal . replace valley) [ (row, column) | row <- [0..height-1], column <- [0..width-1] ]
    where
        height = length valley
        width = length (head valley)
        except = case reflectHorizontal valley of
                    (x:_) -> x
                    _ -> 0

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