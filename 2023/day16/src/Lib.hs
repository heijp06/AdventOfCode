{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Beams(..)
    , parse
    , part1
    , part2
    , reflect
    , reflectAll
    ) where

import Data.List (nub)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf (printf)

type Grid = Map.Map Position Char
type Position = (Int, Int)
type Beam = (Position, Position)
data Beams = Beams { seen :: Set.Set Beam
                   , current :: Set.Set Beam
                   } deriving Show

part1 :: [String] -> Int
part1 xs = length . nub . map fst . Set.toList . seen $ reflectAll grid beam
    where
        grid = parse xs
        beam = ((0, 0), (0, 1))

part2 :: [String] -> Int
part2 = undefined

reflectAll :: Grid -> Beam -> Beams
reflectAll grid beam = head
                     . dropWhile (not . Set.null . current)
                     $ iterate (reflect grid) Beams { seen = Set.empty, current = Set.singleton beam }

reflect :: Grid -> Beams -> Beams
reflect grid Beams{..} = foldr (combineBeams grid) (Beams { current = Set.empty, .. }) current

combineBeams :: Grid -> Beam -> Beams -> Beams
combineBeams grid (pos, dir) beams | pos `Map.notMember` grid =
    addBeam beams (pos, dir) (pos `add` dir, dir)
combineBeams grid (pos, _) beams | grid ! pos == '#' = beams
combineBeams grid beam@(pos, _) beams | grid ! pos `elem` "/\\" =
    addBeam beams beam (mirror (grid ! pos) beam)
combineBeams grid beam@(pos, _) beams | grid ! pos `elem` "-|" =
    addBeams beams beam (split (grid ! pos) beam)
combineBeams grid beam@(pos, _) _ = error $ printf "beam = %s, c = %c" (show beam) (grid ! pos)

addBeam :: Beams -> Beam -> Beam -> Beams
addBeam beams currentBeam newBeam = addBeams beams currentBeam $ Set.singleton newBeam

addBeams :: Beams -> Beam -> Set.Set Beam -> Beams
addBeams Beams{..} currentBeam newBeams =
    Beams { seen = Set.insert currentBeam seen, current = Set.union (Set.difference newBeams seen) current }

mirror :: Char -> Beam -> Beam
mirror '/' ((row, column), (-1, 0)) = ((row, column + 1), (0, 1))
mirror '/' ((row, column), (1, 0)) = ((row, column - 1), (0, -1))
mirror '/' ((row, column), (0, -1)) = ((row + 1, column), (1, 0))
mirror '/' ((row, column), (0, 1)) = ((row - 1, column), (-1, 0))
mirror '\\' ((row, column), (-1, 0)) = ((row, column - 1), (0, -1))
mirror '\\' ((row, column), (1, 0)) = ((row, column + 1), (0, 1))
mirror '\\' ((row, column), (0, -1)) = ((row - 1, column), (-1, 0))
mirror '\\' ((row, column), (0, 1)) = ((row + 1, column), (1, 0))
mirror c beam = error $ printf "Unhandled mirror %c and beam %s" c (show beam)

split :: Char -> Beam -> Set.Set Beam
split '-' ((row, column), (_, 0)) = Set.fromList [ ((row, column + 1), (0, 1)), ((row, column - 1), (0, -1)) ]
split '|' ((row, column), (0, _)) = Set.fromList [ ((row + 1, column), (1, 0)), ((row - 1, column), (-1, 0)) ]
split '-' ((row, column), (0, -1)) = Set.singleton ((row, column - 1), (0, -1))
split '-' ((row, column), (0, 1)) = Set.singleton ((row, column + 1), (0, 1))
split '|' ((row, column), (-1, 0)) = Set.singleton ((row - 1, column), (-1, 0))
split '|' ((row, column), (1, 0)) = Set.singleton ((row + 1, column), (1, 0))
split c beam = error $ printf "Unhandled split %c and beam %s" c (show beam)

add :: Position -> Position -> Position
add (row, column) (dRow, dColumn) = (row + dRow, column + dColumn)

parse :: [String] -> Grid
parse xs = foldr combineGrid border $ zip [ (row, column) | row <- [0..height-1], column <- [0..width-1] ] (concat xs)
    where
        height = length xs
        width = length $ head xs
        top = Map.fromList [ ((-1, column), '#') | column <- [0..width-1] ]
        bottom = Map.fromList [ ((height, column), '#') | column <- [0..width-1] ]
        left = Map.fromList [ ((row, -1), '#') | row <- [0..height-1] ]
        right = Map.fromList [ ((row, width), '#') | row <- [0..height-1] ]
        border = Map.unions [ top, bottom, left, right ]

combineGrid :: (Position, Char) -> Grid -> Grid
combineGrid (_, '.')  grid = grid
combineGrid (pos, c) grid = Map.insert pos c grid