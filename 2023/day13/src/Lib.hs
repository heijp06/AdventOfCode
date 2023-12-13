module Lib
    ( Position
    , parse
    , part1
    , part2
    , reflectHorizontal
    , reflectVertical
    , replace
    ) where

import Data.List (inits, tails, transpose)

type Position = (Int, Int)

part1 :: [String] -> Int
part1 = sum . map reflect . parse

part2 :: [String] -> Int
part2 = undefined

reflect :: [String] -> Int
reflect xs = 100 * reflectHorizontal xs + reflectVertical xs

reflectHorizontal :: [String] -> Int
reflectHorizontal xs = case (top, bottom) of
                        ([], []) -> 0
                        (_, []) -> length (head top) `div` 2
                        ([], _) -> length xs - length (head bottom) `div` 2
                        _ -> error "Reflection from both top and bottom"
    where
        top = [ s | s <- inits xs, not $ null s, even $ length s, s == reverse s ]
        bottom = [ s | s <- tails xs, not $ null s, even $ length s, s == reverse s ]

reflectVertical :: [String] -> Int
reflectVertical = reflectHorizontal . transpose

parse :: [String] -> [[String]]
parse = foldr combineParse [[]]

combineParse :: String -> [[String]] -> [[String]]
combineParse "" acc = [] : acc
combineParse x (ys:acc) = (x:ys) : acc
combineParse x [] = error $ "acc is empty. x = " ++ x

replace :: Position -> [String] -> [String]
replace (row, column) xs = take row xs ++ [newLine] ++ drop (row + 1) xs
    where
        line = xs !! row
        char = line !! column
        newChar = if char == '.' then '#' else '.'
        newLine = take column line ++ [newChar] ++ drop (column + 1) line