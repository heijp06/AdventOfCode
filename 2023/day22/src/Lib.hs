{-# LANGUAGE TupleSections #-}

module Lib
    ( fall
    , parse
    , part1
    , part2
    , step
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Cube = (Int, Int, Int)
type Brick = Set.Set Cube

part1 :: [String] -> Int
part1 = undefined

part2 :: [String] -> Int
part2 = undefined

fall :: Set.Set Brick -> Set.Set Brick
fall bricks = getBricks . head . dropWhile continue $ iterate step (allCubes, bricks, False)
    where
        allCubes = foldr1 Set.union bricks
        continue (_, _, changed) = changed
        getBricks (_, b, _) = b

step :: (Set.Set Cube, Set.Set Brick, Bool) -> (Set.Set Cube, Set.Set Brick, Bool)
step (allCubes, allBricks, changed) = foldr combineBricks (allCubes, allBricks, False) allBricks

combineBricks :: Brick -> (Set.Set Cube, Set.Set Brick, Bool) -> (Set.Set Cube, Set.Set Brick, Bool)
combineBricks brick acc | any (\ (_, _, z) -> z <= 1) brick = acc
combineBricks brick acc@(allCubes, allBricks, _) =
    if Set.disjoint newCubes newBrick
        then (Set.union newCubes newBrick, Set.insert newBrick newBricks, True)
        else acc
    where
        newBrick = Set.map (\ (x, y, z) -> (x, y, z - 1)) brick
        newCubes = Set.difference allCubes brick
        newBricks = Set.delete brick allBricks

parse :: [String] -> Set.Set Brick
parse = Set.fromList . map createBrick

createBrick :: String -> Brick
createBrick xs = case (start, end) of
        ((x1, y1, z1), (x2, y2, z2)) | (x1, y1) == (x2, y2) -> Set.fromList $ map (x1,y1,) [z1..z2]
        ((x1, y1, z1), (x2, y2, z2)) | (x1, z1) == (x2, z2) -> Set.fromList $ map (x1,,z1) [y1..y2]
        ((x1, y1, z1), (x2, y2, z2)) | (y1, z1) == (y2, z2) -> Set.fromList $ map (,y1,z1) [x1..x2]
        _ -> error $ "Unhandled pair: " ++ show (start, end)
    where
        (left, right) = splitInTwo xs
        start = read ("(" ++ left ++ ")") :: Cube
        end = read ("(" ++ right ++ ")") :: Cube

splitInTwo :: String -> (String, String)
splitInTwo xs = case splitOn "~" xs of
                    [left, right] -> (left, right)
                    _ -> error $ "Cannot split in two: " ++ xs