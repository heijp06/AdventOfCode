{-# LANGUAGE TupleSections #-}

module Lib
    ( border
    , double
    , loop
    , next
    , parse
    , part1
    , part2
    , reachable
    , directionsAfterStart
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

type Position = (Int, Int)
type Pipe = (Position, Position)
type Grid = Map.Map Position Pipe

data Result = Result { inside :: Set.Set Position
                     , outside :: Set.Set Position
                     , edge :: Set.Set Position
                     , current :: Set.Set Position
                     , newCurrent :: Set.Set Position
                     } deriving Show

part1 :: [String] -> Int
part1 = (`div` 2) . length . loop

part2 :: [String] -> Int
part2 xs = length . filter (\(x, y) -> even x && even y) . Set.toList $ inside filledResult
    where
        doubleLoop = double $ loop xs
        loopAsSet = Set.fromList doubleLoop
        minX = minimum $ map fst doubleLoop
        maxX = maximum $ map fst doubleLoop
        minY = minimum $ map snd doubleLoop
        maxY = maximum $ map snd doubleLoop
        result = Result { inside = Set.empty
                        , outside = border (minX, minY) (maxX, maxY)
                        , edge = loopAsSet
                        , current = Set.empty
                        , newCurrent = Set.empty
                        }
        filledResult = fill result (minX, minY) (maxX, maxY)

fill :: Result -> Position -> Position -> Result
fill result (minX, minY) (maxX, maxY) = foldr doFill result [ (x, y) | y <- [minY..maxY], x <- [minX..maxX] ]

doFill :: Position -> Result -> Result
doFill pos result | pos `Set.member` inside result = result
doFill pos result | pos `Set.member` outside result = result
doFill pos result | pos `Set.member` edge result = result
doFill pos result = newResult { inside = Set.unions [ inside newResult, current newResult, newCurrent newResult ]
                              , current = Set.empty
                              , newCurrent = Set.empty
                              }
    where
        newResult = last . takeWhile (not . Set.null . newCurrent) $ iterate doFill' (result { newCurrent = Set.singleton pos })

doFill' :: Result -> Result
doFill' result = if Set.disjoint new (outside result)
                    then result { current = Set.union (current result) (newCurrent result)
                                , newCurrent = new }
                    else result { outside = Set.unions [ outside result, current result, newCurrent result, new ]
                                , current = Set.empty
                                , newCurrent = Set.empty
                                }
    where
        new = Set.fromList [ (x + dx, y + dy) | (x, y) <- Set.toList (newCurrent result)
                                              , (dx, dy) <- [(0, -1), (1, 0), (0, 1), (-1, 0)]
                                              , (x + dx, y + dy) `Set.notMember` Set.unions [ edge result, current result, newCurrent result ]]

loop :: [String] -> [Position]
loop xs = start : (takeWhile (/=start) . map fst . drop 1 $ iterate (next grid) (start, add start dir))
    where
        (grid, start) = parse xs
        dir = head $ directionsAfterStart grid start

border :: Position -> Position -> Set.Set Position
border (minX, minY) (maxX, maxY) = Set.unions [ Set.fromList $ map (minX-1,) [minY-1..maxY+1]
                                              , Set.fromList $ map (maxX+1,) [minY-1..maxY+1]
                                              , Set.fromList $ map (,minY-1) [minX-1..maxX+1]
                                              , Set.fromList $ map (,maxY+1) [minX-1..maxX+1]
                                              ]

double :: [Position] -> [Position]
double [] = []
double xs@((x1, y1):_) = tail $ foldr addDouble [(2 * x1, 2 * y1)] xs

addDouble :: Position -> [Position] -> [Position]
addDouble (x1, y1) acc@((x2, y2):_) = (2 * x1, 2 * y1) : ((2 * x1 + x2) `div` 2, (2 * y1 + y2) `div` 2) : acc
addDouble _ [] = error $ "Accumulator can not be empty."

directionsAfterStart :: Grid -> Position -> [Position]
directionsAfterStart grid start = [ direction | direction <- [(0, -1), (1, 0), (0, 1), (-1, 0)]
                                              , reachable grid start direction ]

reachable :: Grid -> Position -> Position -> Bool
reachable grid curr@(x, y) dir@(dx, dy) = result
    where
        target = (x + dx, y + dy)
        (dirCurrent1, dirCurrent2) = Map.findWithDefault noPipe curr grid
        (dirTarget1, dirTarget2) = Map.findWithDefault noPipe target grid
        result = dir `elem` [dirCurrent1, dirCurrent2] && (dir == neg dirTarget1 || dir == neg dirTarget2)

noPipe :: Pipe
noPipe = ((0, 0), (0, 0))

next :: Grid -> (Position, Position) -> (Position, Position)
next grid (curr, previous) = head [ (add curr d, curr) | d <- [(0, -1), (1, 0), (0, 1), (-1, 0)]
                                                                , reachable grid curr d
                                                                , add curr d /= previous
                                                                ]

add :: Position -> Position -> Position
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

neg :: Position -> Position
neg (x, y) = (-x, -y)

parse :: [String] -> (Grid, Position)
parse xs = case dirs of
            [dir1, dir2] -> (Map.insert start (dir1, dir2) grid, start)
            _ -> error $ "Cannot build pipe for start: " ++ show dirs
    where
        (grid, start) = parse' xs
        dirs = [ d
               | d <- [(0, 1), (1, 0), (0, -1), (-1, 0)]
               , let (d1, d2) = Map.findWithDefault noPipe (add start d) grid in d1 == neg d || d2 == neg d
               ]

parse' :: [String] -> (Grid, Position)
parse' xs = foldr addTile (Map.empty, (-1, -1)) $ zip [ (x, y) | y <- [0..height-1], x <- [0..width-1]] $ concat xs
    where
        height = length xs
        width = length $ head xs

addTile :: (Position, Char) -> (Grid, Position) -> (Grid, Position)
addTile (pos, tile) (grid, start) = case tile of
                                        '|' -> insert ((0, -1), (0, 1))
                                        '-' -> insert ((-1, 0), (1, 0))
                                        'L' -> insert ((0, -1), (1, 0))
                                        'J' -> insert ((-1, 0), (0, -1))
                                        '7' -> insert ((-1, 0), (0, 1))
                                        'F' -> insert ((0, 1), (1, 0))
                                        '.' -> (grid, start)
                                        'S' -> (grid, pos)
                                        _ -> error $ "Unexpected tile: " ++ show tile
    where
        insert pipe = (Map.insert pos pipe grid, start)