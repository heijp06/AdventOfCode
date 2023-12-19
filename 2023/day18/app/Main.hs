module Main ( input
            , main
            ) where

-- import Data.Foldable (for_)
import Lib

main :: IO ()
main = do
    xs <- input
    -- for_ (rows xs) print
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs
  
input :: IO [String]
input = lines <$> readFile "..\\..\\data\\2023\\day18.txt"

