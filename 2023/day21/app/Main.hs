module Main ( input
            , main
            ) where

import Data.Foldable (for_)
import Lib

main :: IO ()
main = do
    xs <- input
    -- putStr "Part 1: "
    -- print $ part1 xs
    -- putStr "Part 2: "
    -- print $ part2 xs
    for_ (display 196 xs) putStrLn
  
input :: IO [String]
input = lines <$> readFile "..\\..\\data\\2023\\day21.txt"

