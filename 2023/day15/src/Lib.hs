{-# LANGUAGE TupleSections #-}

module Lib
    ( build
    , hash
    , parse
    , part1
    , part2
    ) where

import Data.Char (digitToInt, isLower, ord)
import Data.Function (on)
import Data.List (deleteBy, findIndex)
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as Map

data Cmd = Set String Int | Del String deriving (Eq, Ord, Show)
type Lens = (String, Int)
type HashMap = Map.Map Int [Lens]

part1 :: [String] -> Int
part1 = sum . map hash . splitOn "," . head

part2 :: [String] -> Int
part2 = sum . map focusingPower . Map.toList . build . map parse . splitOn "," . head

hash :: String -> Int
hash = foldl combineHash 0

focusingPower :: (Int, [Lens]) -> Int
focusingPower (box, lenses) = sum . map ((box+1)*) $ zipWith (*) [1..] (map snd lenses)

combineHash :: Int -> Char -> Int
combineHash acc = (`mod`256) . (17*) . (acc+) . ord

build :: [Cmd] -> HashMap
build = foldl combineHashMap newHashMap

newHashMap :: HashMap
newHashMap = Map.fromList $ map (,[]) [0..255]

combineHashMap :: HashMap -> Cmd -> HashMap
combineHashMap hashMap (Del label) = Map.insert h newBox hashMap
    where
        h = hash label
        box = hashMap ! h
        newBox = deleteBy ((==) `on` fst) (label, undefined) box
combineHashMap hashMap (Set label focalLength) = Map.insert h newBox hashMap
    where
        h = hash label
        box = hashMap ! h
        newBox = case findIndex ((==label) . fst) box of
                    Just index -> let (start, end) = splitAt index box in
                                    start ++ [(label, focalLength)] ++ tail end
                    Nothing -> box ++ [(label, focalLength)]

parse :: String -> Cmd
parse xs = case operation of
            '=' -> Set label focalLength
            '-' -> Del label
            _ -> error $ "Unknown operation: " ++ [operation]
    where
        label = takeWhile isLower xs
        lastChar = last xs
        operation = if lastChar == '-' then '-' else '='
        focalLength = digitToInt $ last xs