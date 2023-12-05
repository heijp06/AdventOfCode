{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Mapping(..)
    , Range (..)
    , parse
    , part1
    , part2
    ) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

data Range = Range { destination :: Int
                   , source :: Int
                   , length :: Int
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
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

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
