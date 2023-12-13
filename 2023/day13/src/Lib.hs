module Lib
    ( Position
    , parse
    , part1
    , part2
    , reflect
    , reflectHorizontal
    , reflectVertical
    , replace
    , smudge
    ) where

import Data.Bifunctor (first, second)
import Data.List (inits, tails, transpose)
import Text.Printf (printf)

type Position = (Int, Int)
type Valley = [String]

part1 :: [String] -> Int
part1 = sum . concatMap (map snd . reflect) . parse

part2 :: [String] -> Int
part2 = sum . map smudge . parse

reflect :: Valley -> [(Valley, Int)]
reflect xs = map (second (*100)) (reflectHorizontal xs) ++ reflectVertical xs

reflectHorizontal :: Valley -> [(Valley, Int)]
reflectHorizontal xs = top ++ bottom
    where
        top = [ (xs, length s `div` 2) | s <- inits xs, not $ null s, even $ length s, s == reverse s ]
        bottom = [ (xs, length xs - length s `div` 2) | s <- tails xs, not $ null s, even $ length s, s == reverse s ]

reflectVertical :: Valley -> [(Valley, Int)]
reflectVertical = map (first transpose) . reflectHorizontal . transpose

parse :: [String] -> [Valley]
parse = foldr combineParse [[]]

smudge :: Valley -> Int
smudge valley = (`div` 2) . sum . filter (/=except) $ concatMap (map snd . reflect . replace valley) [ (row, column) | row <- [0..height-1], column <- [0..width-1] ]
    where
        height = length valley
        width = length (head valley)
        except = case reflect valley of
                    ((_, x):_) -> x
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