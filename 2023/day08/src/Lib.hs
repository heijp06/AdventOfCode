module Lib
    ( parse
    , part1
    , part2
    ) where

import Data.Map ((!))
import qualified Data.Map as Map

type Network = Map.Map String (String, String)

part1 :: [String] -> Int
part1 = solve "AAA"

part2 :: [String] -> Int
part2 xs = foldr1 lcm [ solve start xs | start <- Map.keys network, last start == 'A' ]
    where
        (_, network) = parse xs

solve :: String -> [String] -> Int
solve start xs = step network 0 start $ cycle directions
    where
        (directions, network) = parse xs

step :: Network -> Int -> String -> String -> Int
step _ _ _ [] = error "List of directions should be infinitely long."
step network steps current (x:xs) = if last next == 'Z'
                                        then steps + 1
                                        else step network (steps + 1) next xs
    where
        (left, right) = network ! current
        next = case x of
                'L' -> left
                'R' -> right
                _ -> error $ "Invalid direction: " ++ [x]

parse :: [String] -> (String, Network)
parse xs = (directions, network)
    where
        directions = head xs
        network = Map.fromList [ (take 3 x, (take 3 $ drop 7 x, take 3 $ drop 12 x)) | x <- tail $ tail xs ]