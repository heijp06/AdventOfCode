module Lib
    ( hash
    , part1
    , part2
    ) where

import Data.Char (ord)
import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 = sum . map hash . splitOn "," . head

part2 :: [String] -> Int
part2 = undefined

hash :: String -> Int
hash = foldl combineHash 0

combineHash :: Int -> Char -> Int
combineHash acc = (`mod`256) . (17*) . (acc+) . ord