{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Crucible(..)
    , initialPuzzleState
    , part1
    , part2
    , parse
    , step
    ) where

import Data.Array ((!), Array, array, bounds, inRange, range)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.PQueue.Min as PQueue

type Position = (Int, Int)
type Direction = (Int, Int)
type Grid = Array Position Int
type Queue = PQueue.MinQueue Crucible

data Crucible = Crucible { minHeatLoss :: Int
                         , currentHeatLoss :: Int
                         , motionState :: MotionState
                         } deriving (Eq, Ord, Show)

data MotionState = MotionState { position :: Position
                               , direction :: Direction
                               , timesStraight :: Int
                               } deriving (Eq, Ord, Show)

data PuzzleState = PuzzleState { globalMinHeatLoss :: Int
                               , seen :: Map.Map MotionState Int
                               , queue :: Queue
                               } deriving Show

part1 :: [String] -> Int
part1 xs = globalMinHeatLoss
         . head
         . dropWhile (not . PQueue.null . queue)
         $ iterate (step grid) (initialPuzzleState grid)
    where
        grid = parse xs

part2 :: [String] -> Int
part2 = undefined

step :: Grid -> PuzzleState -> PuzzleState
step _ (PuzzleState _ _ queue) | PQueue.null queue = error "Queue is empty."
step grid PuzzleState{..} = step' grid PuzzleState { queue = newQueue, .. } crucible
    where
        (crucible, newQueue) = PQueue.deleteFindMin queue 

step' :: Grid -> PuzzleState -> Crucible -> PuzzleState
-- Stop if globalMinHeatLoss is less or equal than anything that can still come.
step' _ PuzzleState{..} (Crucible minHeatLoss _ _)
    | globalMinHeatLoss <= minHeatLoss = PuzzleState { queue = PQueue.empty, .. }
-- Do not move more than 3 times in the same direction.
step' _ puzzleState (Crucible _ _ (MotionState _ _ timesStraight))
    | timesStraight >= 3 = puzzleState
-- Do not move off grid.
step' grid puzzleState (Crucible _ _ (MotionState position direction _))
    | not $ inRange (bounds grid) newPosition = puzzleState
    where
        newPosition = move position direction
-- Do not move to a city block that was reached before with less or equal heat loss
step' grid puzzleState@(PuzzleState{..}) (Crucible _ currentHeatLoss MotionState{..})
    | isJust currentLoss && fromJust currentLoss <= newLoss = puzzleState
    where
        newPosition = move position direction
        newLoss = currentHeatLoss + (grid ! newPosition)
        currentLoss = Map.lookup (MotionState { position = newPosition, .. }) seen
-- Do not move further after the bottom right corner is reached.
step' grid PuzzleState{..} (Crucible _ currentHeatLoss MotionState{..})
    | newPosition == corner = PuzzleState { globalMinHeatLoss = min globalMinHeatLoss newLoss, .. }
    where
        newPosition = move position direction
        (_, corner) = bounds grid
        newLoss = currentHeatLoss + (grid ! newPosition)
-- Move to new city block and plan next steps.
step' grid PuzzleState{..} (Crucible _ currentHeatLoss MotionState{..})
    = PuzzleState { seen = newSeen, queue = qStraight, .. }
    where
        newPosition = move position direction
        newLoss = currentHeatLoss + (grid ! newPosition)
        currentLoss = Map.lookup (MotionState { position = newPosition, .. }) seen
        newSeen = if isNothing currentLoss || newLoss < fromJust currentLoss
                    then Map.insert (MotionState { position = newPosition, .. }) newLoss seen
                    else seen
        newMinHeatLoss = newLoss + minTotalLoss grid newPosition
        (dRow, dColumn) = direction
        left = MotionState newPosition (-dColumn, dRow) 0
        right = MotionState newPosition (dColumn, -dRow) 0
        straight = MotionState newPosition (dRow, dColumn) (timesStraight + 1)
        qLeft = PQueue.insert (Crucible newMinHeatLoss newLoss left) queue
        qRight = PQueue.insert (Crucible newMinHeatLoss newLoss right) qLeft
        qStraight = PQueue.insert (Crucible newMinHeatLoss newLoss straight) qRight

move :: Position -> Direction -> Position
move (row, column) (dRow, dColumn) = (row + dRow, column + dColumn)

initialPuzzleState :: Grid -> PuzzleState
initialPuzzleState grid = PuzzleState (maxTotalLoss grid (0, 0)) Map.empty (initialQueue grid)

initialQueue :: Grid -> Queue
initialQueue grid = PQueue.fromList [ Crucible (minTotalLoss grid (0, 0)) 0 $ MotionState (0, 0) (1, 0) 0
                                    , Crucible (minTotalLoss grid (0, 0)) 0 $ MotionState (0, 0) (0, 1) 0
                                    ]

parse :: [String] -> Grid
parse xs = array indexes $ zip (range indexes) (map digitToInt $ concat xs)
    where
        indexes = ((0, 0), (length xs - 1, length (head xs) - 1))

maxTotalLoss :: Grid -> Position -> Int
maxTotalLoss grid position = 9 * minTotalLoss grid position

minTotalLoss :: Grid -> Position -> Int
minTotalLoss grid (row, column) = maxRow - row + maxColumn - column
    where
        ((_, _), (maxRow, maxColumn)) = bounds grid