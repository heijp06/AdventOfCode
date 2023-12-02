{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Game

part1 :: [String] -> Int
part1 xs = sum [ identifier game | game <- map parse xs, all possible (draws game) ]

part2 :: [String] -> Int
part2 = sum . map (power . foldr1 add . draws . parse)

possible :: Draw -> Bool
possible Draw{..} = red <= 12 && green <= 13 && blue <= 14

add :: Draw -> Draw -> Draw
add draw1 draw2 = Draw { red = choose red
                       , green = choose green
                       , blue = choose blue
                       }
    where
        choose color = max (color draw1) (color draw2)

power :: Draw -> Int
power Draw{..} = red * green * blue