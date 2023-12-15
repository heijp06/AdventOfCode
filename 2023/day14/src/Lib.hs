{-# LANGUAGE TupleSections #-}

module Lib
    ( cubesByColumns
    , cubesByRows
    , cycle
    , positionsOfRocks
    , part1
    , part2
    , period
    , tiltNorth
    , tiltWest
    , tiltSouth
    , tiltEast
    , ps
    ) where

import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Map ((!))
import qualified Data.Map as Map
import Prelude hiding (cycle)

type Position = (Int, Int)

part1 :: [String] -> Int
part1 xs = sum . map ((height-) . fst) $ tiltNorth cubes rocks
    where
        cubes = cubesByColumns xs
        rocks = positionsOfRocks xs
        height = length xs

part2 :: [String] -> Int
part2 xs = sum . map ((height-) . fst) . (!! 109) $ iterate c rocks
    where
        cubeCols = cubesByColumns xs
        cubeRows = cubesByRows xs
        rocks = positionsOfRocks xs
        c = cycle cubeCols cubeRows
        height = length xs

ps :: [String] -> [[Position]]
ps xs = take 100 $ iterate c rocks
    where
        cubeCols = cubesByColumns xs
        cubeRows = cubesByRows xs
        rocks = positionsOfRocks xs
        c = cycle cubeCols cubeRows

period :: [String] -> (Int, Int)
period xs = (Map.size m, m ! r)
    where
        cubeCols = cubesByColumns xs
        cubeRows = cubesByRows xs
        rocks = positionsOfRocks xs
        c = cycle cubeCols cubeRows
        (m, r) = head . dropWhile (\ (m, ps) -> ps `Map.notMember` m) $ iterate (p c) (Map.empty, rocks)

p :: ([Position] -> [Position]) -> (Map.Map [Position] Int, [Position]) -> (Map.Map [Position] Int, [Position])
p c (m, ps) = (Map.insert ps (Map.size m) m, c ps)

cycle :: Map.Map Int [Int] -> Map.Map Int [Int] -> [Position] -> [Position]
cycle cubeCols cubeRows rocks = rocksEast
    where
        rocksNorth = tiltNorth cubeCols rocks
        rocksWest = tiltWest cubeRows rocksNorth
        rocksSouth = tiltSouth cubeCols rocksWest
        rocksEast = tiltEast cubeRows rocksSouth

tiltNorth :: Map.Map Int [Int] -> [Position] -> [Position]
tiltNorth cubes rocks = concatMap (\ ((r, c), n) -> map (,c) [r+1..r+n]) $ Map.toAscList m
    where
        m = foldr f Map.empty rocks
        f (row, col) = Map.insertWith (+) (fromJust $ find (<row) (cubes ! col), col) 1

tiltWest :: Map.Map Int [Int] -> [Position] -> [Position]
tiltWest cubes rocks = concatMap (\ ((r, c), n) -> map (r,) [c+1..c+n]) $ Map.toAscList m
    where
        m = foldr f Map.empty rocks
        f (row, col) = Map.insertWith (+) (row, fromJust $ find (<col) (cubes ! row)) 1

tiltSouth :: Map.Map Int [Int] -> [Position] -> [Position]
tiltSouth cubes rocks = concatMap (\ ((r, c), n) -> map (,c) [r-n..r-1]) $ Map.toAscList m
    where
        m = foldr f Map.empty rocks
        f (row, col) = Map.insertWith (+) (fromJust $ find (>row) (reverse (cubes ! col)), col) 1

tiltEast :: Map.Map Int [Int] -> [Position] -> [Position]
tiltEast cubes rocks = concatMap (\ ((r, c), n) -> map (r,) [c-n..c-1]) $ Map.toAscList m
    where
        m = foldr f Map.empty rocks
        f (row, col) = Map.insertWith (+) (row, fromJust $ find (>col) (reverse (cubes ! row))) 1

cubesByColumns :: [String] -> Map.Map Int [Int]
cubesByColumns xs = Map.map (height:) . foldl addCube columns $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs
        columns = Map.fromList $ map (,[-1]) [0..width-1]

addCube :: Map.Map Int [Int] -> (Position, Char) -> Map.Map Int [Int]
addCube acc ((row, column), '#') = Map.insertWith (++) column [row] acc
addCube acc _ = acc

cubesByRows :: [String] -> Map.Map Int [Int]
cubesByRows xs = Map.map (width:) . foldl addCube' rows $
            zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs
        rows = Map.fromList $ map (,[-1]) [0..height-1]

addCube' :: Map.Map Int [Int] -> (Position, Char) -> Map.Map Int [Int]
addCube' acc ((row, column), '#') = Map.insertWith (++) row [column] acc
addCube' acc _ = acc

positionsOfRocks :: [String] -> [Position]
positionsOfRocks xs = map fst . filter ((=='O') . snd) $ zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs
