module Main ( input
            , main
            ) where

import Examples
import Lib

main :: IO ()
main = do
    print $ ps example1
    xs <- input
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs
  
input :: IO [String]
input = lines <$> readFile "..\\..\\data\\2023\\day14.txt"

