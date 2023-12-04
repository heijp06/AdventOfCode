module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Text.Printf (printf)

part1 :: [String] -> Int
part1 = sum . map score

part2 :: [String] -> Int
part2 = fst . foldl add (0, [])

score :: String -> Int
score xs = let c = count xs in if c == 0 then 0 else 2 ^ (c - 1)

add :: (Int, [Int]) -> String -> (Int, [Int])
add (total, toAdd) xs = (total + cards, newToAdd)
    where
        cards = count xs + copies
        copies = if null toAdd then 0 else sum toAdd
        newToAdd
            | cards == 0 = []
            | null toAdd = [cards]
            | otherwise = cards : [ c - 1 | c <- toAdd, c > 1 ]

count :: String -> Int
count xs = Set.size $ Set.intersection winners actual
    where
        (_, values) = splitInTwo ": " xs
        (left, right) = splitInTwo " | " values
        winners = Set.fromList . filter (not . null) $ splitOn " " left
        actual = Set.fromList . filter (not . null) $ splitOn " " right

splitInTwo :: String -> String -> (String, String)
splitInTwo delimiter xs = case splitOn delimiter xs of
    [left, right] -> (left, right)
    _ -> error $ printf "%s does not split %s in two." delimiter xs
