{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Lib
    ( part1
    , part2
    ) where

import Data.Char (isDigit, digitToInt)
import Data.Function (on)
import Data.List (minimumBy, maximumBy)
import Data.Text (Text, pack)
import Data.Text.Internal.Search (indices)

digitMap :: [(Text, Int)]
digitMap = [ ("one", 1)
           , ("1", 1)
           , ("two", 2)
           , ("2", 2)
           , ("three", 3)
           , ("3", 3)
           , ("four", 4)
           , ("4", 4)
           , ("five", 5)
           , ("5", 5)
           , ("six", 6)
           , ("6", 6)
           , ("seven", 7)
           , ("7", 7)
           , ("eight", 8)
           , ("8", 8)
           , ("nine", 9)
           , ("9", 9)
           ]

part1 :: [String] -> Int
part1 = foldr add 0

part2 :: [String] -> Int
part2 = foldr count 0

count :: String -> Int -> Int
count xs acc = acc + (10 * left) + right
    where
        left = snd . minimumBy (compare `on` fst) $ digitsWithIndices xs
        right = snd . maximumBy (compare `on` fst) $ digitsWithIndices xs

add :: String -> Int -> Int
add xs acc = acc + (10 * head digits) + last digits
    where
        digits = [ digitToInt c | c <- xs, isDigit c ]

digitsWithIndices :: String -> [(Int, Int)]
digitsWithIndices xs = foldr (addIndices $ pack xs) [] digitMap

addIndices :: Text -> (Text, Int) -> [(Int, Int)] -> [(Int, Int)]
addIndices line (name, value) acc = map (, value) (indices name line) ++ acc