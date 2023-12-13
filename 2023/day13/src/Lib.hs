module Lib
    ( parse
    , part1
    , part2
    ) where

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

parse :: [String] -> [[String]]
parse = foldr combine [[]]

combine :: String -> [[String]] -> [[String]]
combine "" acc = [] : acc
combine x (ys:acc) = (x:ys) : acc
combine x [] = error $ "acc is empty. x = " ++ x