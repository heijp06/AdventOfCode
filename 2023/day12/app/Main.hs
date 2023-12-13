module Main ( input
            , main
            ) where

import Lib

main :: IO ()
main = do
    xs <- lines <$> readFile "..\\..\\data\\2023\\day12.txt"
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs

input :: IO [String]
input = lines <$> readFile "..\\..\\data\\2023\\day12.txt"
