{-# LANGUAGE TupleSections #-}

module Lib
    ( Brick(..)
    , fall
    , parse
    , part1
    , part2
    , step
    , supports
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Cube = (Int, Int, Int)
data Brick = Brick { cubes :: Set.Set Cube
                   , fallen :: Bool
                   } deriving Show

instance Eq Brick where
    b1 == b2 = cubes b1 == cubes b2

instance Ord Brick where
    b1 `compare` b2 = cubes b1 `compare` cubes b2

part1 :: [String] -> Int
part1 xs = number - Set.size supporting
    where
        number = length xs
        bricks = parse xs
        supporting = supports $ fall bricks

part2 :: [String] -> Int
part2 xs = sum $ Set.map (disintegrate stable) supporting
    where
        bricks = parse xs
        stable = Set.map (\ brick -> brick { fallen = False }) $ fall bricks
        supporting = supports stable

disintegrate :: Set.Set Brick -> Brick -> Int
disintegrate bricks brick = Set.size . Set.filter fallen $ fall newBricks
    where
        newBricks = Set.delete brick bricks

supports :: Set.Set Brick -> Set.Set Brick
supports bricks = foldr (combineSupports bricks) Set.empty bricks

combineSupports :: Set.Set Brick -> Brick -> Set.Set Brick -> Set.Set Brick
combineSupports bricks brick acc =
    if Set.size supporting == 1
        then Set.union supporting acc
        else acc
    where
        newBricks = Set.delete brick bricks
        newBrick = Brick { cubes = Set.map (\ (x, y, z) -> (x, y, z - 1)) (cubes brick)
                         , fallen = True
                         }
        supporting = Set.filter (not . Set.disjoint (cubes newBrick) . cubes) newBricks

fall :: Set.Set Brick -> Set.Set Brick
fall bricks = getBricks . head . dropWhile continue $ iterate step (allCubes, bricks, True)
    where
        allCubes = foldr1 Set.union $ Set.map cubes bricks
        continue (_, _, changed) = changed
        getBricks (_, b, _) = b

step :: (Set.Set Cube, Set.Set Brick, Bool) -> (Set.Set Cube, Set.Set Brick, Bool)
step (allCubes, allBricks, _) = foldr combineBricks (allCubes, allBricks, False) allBricks

combineBricks :: Brick -> (Set.Set Cube, Set.Set Brick, Bool) -> (Set.Set Cube, Set.Set Brick, Bool)
combineBricks brick acc | any (\ (_, _, z) -> z <= 1) (cubes brick) = acc
combineBricks brick acc@(allCubes, allBricks, _) =
    if Set.disjoint newCubes newBrick
        then (Set.union newCubes newBrick, Set.insert (Brick newBrick True) newBricks, True)
        else acc
    where
        newBrick = Set.map (\ (x, y, z) -> (x, y, z - 1)) (cubes brick)
        newCubes = Set.difference allCubes (cubes brick)
        newBricks = Set.delete brick allBricks

parse :: [String] -> Set.Set Brick
parse = Set.fromList . map createBrick

createBrick :: String -> Brick
createBrick xs = case (start, end) of
        ((x1, y1, z1), (x2, y2, z2)) | (x1, y1) == (x2, y2) -> Brick (Set.fromList $ map (x1,y1,) [z1..z2]) False
        ((x1, y1, z1), (x2, y2, z2)) | (x1, z1) == (x2, z2) -> Brick (Set.fromList $ map (x1,,z1) [y1..y2]) False
        ((x1, y1, z1), (x2, y2, z2)) | (y1, z1) == (y2, z2) -> Brick (Set.fromList $ map (,y1,z1) [x1..x2]) False
        _ -> error $ "Unhandled pair: " ++ show (start, end)
    where
        (left, right) = splitInTwo xs
        start = read ("(" ++ left ++ ")") :: Cube
        end = read ("(" ++ right ++ ")") :: Cube

splitInTwo :: String -> (String, String)
splitInTwo xs = case splitOn "~" xs of
                    [left, right] -> (left, right)
                    _ -> error $ "Cannot split in two: " ++ xs