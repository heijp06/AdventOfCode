{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Lib
    ( part1
    , part2
    ) where

import Data.Function (on)
import Data.List (minimumBy, maximumBy)
import Data.Text (Text, pack)
import Data.Text.Internal.Search (indices)

type DigitMap = [(Text, Int)]

numericDigits :: DigitMap
numericDigits = [ ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9) ]

textDigits :: DigitMap
textDigits = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5)
             , ("six", 6), ("seven", 7) , ("eight", 8) , ("nine", 9) ]

part1 :: [String] -> Int
part1 = solve numericDigits

part2 :: [String] -> Int
part2 = solve $ numericDigits ++ textDigits

solve :: DigitMap -> [String] -> Int
solve digitMap = foldr (count digitMap) 0

count :: DigitMap -> String -> Int -> Int
count digitMap xs acc = acc + (10 * left) + right
    where
        left = snd . minimumBy (compare `on` fst) $ digitsWithIndices xs digitMap
        right = snd . maximumBy (compare `on` fst) $ digitsWithIndices xs digitMap

digitsWithIndices :: String -> DigitMap -> [(Int, Int)]
digitsWithIndices xs = foldr (addIndices $ pack xs) []

addIndices :: Text -> (Text, Int) -> [(Int, Int)] -> [(Int, Int)]
addIndices line (name, value) acc = map (, value) (indices name line) ++ acc