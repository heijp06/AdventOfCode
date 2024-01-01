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
    putStrLn "Go to https://www.wolframalpha.com/input?i=system+equation+calculator."
    putStrLn ""
    putStrLn "Plug in the first 4 equations to get x, y, u and v."
    putStrLn "In the second 2 equations replace x and u with the now known values and get z and w."
    putStrLn "Add x, y and z."
    putStrLn ""
    for_ (part2 xs) putStrLn
  
input :: IO [String]
input = lines <$> readFile "..\\..\\data\\2023\\day24.txt"

