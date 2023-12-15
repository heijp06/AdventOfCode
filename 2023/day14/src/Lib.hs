{-# LANGUAGE TupleSections #-}

module Lib
    ( cubesByColumns
    , fall
    , part1
    , part2
    , rocksByColumns
    , tilt
    ) where

import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type Position = (Int, Int)

part1 :: [String] -> Int
part1 xs = sum . map (height-) . concat $ tilt cubes rocks
    where
        cubes = cubesByColumns xs
        rocks = rocksByColumns xs
        height = length xs

part2 :: [String] -> Int
part2 = undefined

-- columnsToRows :: Int -> [[Int]] -> [[Int]]
-- columnsToRows height iss = undefined
--     where
--         rows = Map.fromList $ map (,[-1]) [0..height-1]
--         x = foldl addColumn rows $ zip [0..] iss
--         addColumn r (col, rs) = 

tilt :: [[Int]] -> [[Int]] -> [[Int]]
tilt = zipWith fall

fall :: [Int] -> [Int] -> [Int]
fall cubes rocks = concatMap (\ (r, n) -> [r+1..r+n]) $ Map.toAscList m
    where
        m = foldr f Map.empty rocks
        f rock = Map.insertWith (+) (fromJust $ find (<rock) cubes) 1

cubesByColumns :: [String] -> [[Int]]
cubesByColumns xs = map ((width:) . snd) . Map.toAscList . foldl addCube columns $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs
        columns = Map.fromList $ map (,[-1]) [0..width-1]

addCube :: Map.Map Int [Int] -> (Position, Char) -> Map.Map Int [Int]
addCube acc ((row, column), '#') = Map.insertWith (++) column [row] acc
addCube acc _ = acc

rocksByColumns :: [String] -> [[Int]]
rocksByColumns xs = map snd . Map.toAscList . foldl addRock columns $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs
        columns = Map.fromList $ map (,[]) [0..width-1]

addRock :: Map.Map Int [Int] -> (Position, Char) -> Map.Map Int [Int]
addRock acc ((row, column), 'O') = Map.insertWith (++) column [row] acc
addRock acc _ = acc