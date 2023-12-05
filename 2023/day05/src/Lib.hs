{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Mapping(..)
    , Pair
    , Range (..)
    , mapCategories
    , mapRanges
    , parse
    , part1
    , part2
    , seedRanges
    , seeds
    , splitRange
    , splitRanges
    ) where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.List.Split (splitOn)

type Pair = (Int, Int)

data Range = Range { destination :: Int
                   , source :: Int
                   , len :: Int
                   } deriving (Eq, Show)

data Mapping = Mapping { seedToSoil :: [Range]
                       , soilToFertilizer :: [Range]
                       , fertilizerToWater :: [Range]
                       , waterToLight :: [Range]
                       , lightToTemperature :: [Range]
                       , temperatureToHumidity :: [Range]
                       , humidityToLocation :: [Range]
                       } deriving (Eq, Show)

part1 :: [String] -> Int
part1 xs = minimum . map (`mapCategories` parse xs) . seeds $ head xs

part2 :: [String] -> Int
part2 xs = minimum . map fst . splitCategories (parse xs) . seedRanges $ head xs

parse :: [String] -> Mapping
parse = snd . foldl add ("", Mapping [] [] [] [] [] [] []) . filter (not . null) . tail

add :: (String, Mapping) -> String -> (String, Mapping)
add (fs, mapping@Mapping{..}) xs = if isDigit (head xs)
                                    then (fs, update fs (range xs))
                                    else (xs, mapping)

    where
        update "seed-to-soil map:" r = mapping { seedToSoil = seedToSoil ++ [r] }
        update "soil-to-fertilizer map:" r = mapping { soilToFertilizer = soilToFertilizer ++ [r] }
        update "fertilizer-to-water map:" r = mapping { fertilizerToWater = fertilizerToWater ++ [r] }
        update "water-to-light map:" r = mapping { waterToLight = waterToLight ++ [r] }
        update "light-to-temperature map:" r = mapping { lightToTemperature = lightToTemperature ++ [r] }
        update "temperature-to-humidity map:" r = mapping { temperatureToHumidity = temperatureToHumidity ++ [r] }
        update "humidity-to-location map:" r = mapping { humidityToLocation = humidityToLocation ++ [r] }
        update gs _ = error $ "Unexpected header: " ++ gs

range :: String -> Range
range xs = case splitOn " " xs of
            [d, s, l] -> Range (read d) (read s) (read l)
            _ -> error $ "Cannot parse as range: " ++ xs

mapRanges :: Int -> [Range] -> Int
mapRanges n = foldl go n
    where
        go acc Range{..} = if n >= source && n - source < len
                                then destination + n - source
                                else acc

mapCategories :: Int -> Mapping -> Int
mapCategories n Mapping{..} = foldl mapRanges n [ seedToSoil
                                                , soilToFertilizer
                                                , fertilizerToWater
                                                , waterToLight
                                                , lightToTemperature
                                                , temperatureToHumidity
                                                , humidityToLocation
                                                ]

seeds :: String -> [Int]
seeds = map read . splitOn " " . drop (length "seeds: ")

seedRanges :: String -> [Pair]
seedRanges xs = zip evens odds
    where
        ss = zip ([0..] :: [Int]) $ seeds xs
        evens = [ s | (i, s) <- ss, even i ]
        odds = [ s | (i, s) <- ss, odd i ]

splitRange :: Range -> Pair -> ([Pair], [Pair])
splitRange Range{..} (start, l)
    | start + l <= source = ([ (start, l) ], [])
    | start >= source + len = ([ (start, l) ], [])
    | start >= source && start + l <= source + len = ([], [ (destination + start - source, l) ])
    | start < source && start + l <= source + len = ([ (start, source - start) ], [ (destination, l - source + start) ])
    | start >= source = ([ ( source + len, start + l - source - len) ], [ (destination + start - source, source + len - start) ])
    | otherwise = ([ (start, source - start), (source + len, start + l - source - len) ], [ (destination, len) ])

splitRanges :: [Range] -> [Pair] -> [Pair]
splitRanges rs pairs = liftA2 (++) fst snd $ foldl go (pairs, []) rs
    where
        go (toSplit, done) r = let xs = map (splitRange r) toSplit in (concatMap fst xs, concatMap snd xs ++ done)

splitCategories :: Mapping -> [Pair] -> [Pair]
splitCategories Mapping{..} pairs = foldl (flip splitRanges) pairs [ seedToSoil
                                                                   , soilToFertilizer
                                                                   , fertilizerToWater
                                                                   , waterToLight
                                                                   , lightToTemperature
                                                                   , temperatureToHumidity
                                                                   , humidityToLocation
                                                                   ]