module Lib
    ( parse
    , part1
    , part2
    ) where

import Data.Map ((!))
import qualified Data.Map as Map

type Network = Map.Map String (String, String)

part1 :: [String] -> Int
part1 xs = fst . foldl (step network) (0, "AAA") $ cycle directions
    where
        (directions, network) = parse xs

part2 :: [String] -> Int
part2 = undefined

step :: Network -> (Int, String) -> Char -> (Int, String)
step = undefined

parse :: [String] -> (String, Network)
parse xs = (directions, network)
    where
        directions = head xs
        network = Map.fromList [ (take 3 x, ((take 3 . drop 7) x, (take 3 . drop 12) x)) | x <- (tail . tail) xs ]