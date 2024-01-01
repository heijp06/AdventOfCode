module Main ( input
            , main
            ) where

import Data.Foldable (for_)

import Lib

main :: IO ()
main = do
    xs <- input
    putStr "Part 1: "
    print $ part1 xs
    putStrLn "Part 2: "
    for_ (part2 xs) putStrLn
  
input :: IO [String]
input = lines <$> readFile "..\\..\\data\\2023\\day24.txt"

