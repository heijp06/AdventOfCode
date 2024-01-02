module Lib
    ( bounds
    , parse
    , part1
    , part2
    , solve
    , union
    ) where

import Data.List (delete, find, groupBy, sort)
import Data.Range ((+=+), Bound(..), BoundType(..), Range(..))
import qualified Data.Range as Range

type Position = (Int, Int)
type ColumnPair = (Int, Int)
type Row = (Int, [ColumnPair])

part1 :: [String] -> Int
part1 = solve

part2 :: [String] -> Int
part2 = undefined

solve :: [String] -> Int
solve = fst . foldl combineSolve (0, (0, [])) . parse

combineSolve :: (Int, Row) -> Row -> (Int, Row)
combineSolve (accSize, (accRow, accRanges)) (row, ranges)
    = (accSize + sizeOfPreviousRows + sizeOfCurrentRow, (row, buildNewRanges accRanges ranges))
    where
        sizeOfPreviousRows = (row - accRow - 1) * rangeLength accRanges
        sizeOfCurrentRow = rangeLength $ union accRanges ranges

buildNewRanges :: [ColumnPair] -> [ColumnPair] -> [ColumnPair]
buildNewRanges = foldr addRange

addRange :: ColumnPair -> [ColumnPair] -> [ColumnPair]
addRange pair@(left, right) pairs
    = let matches = (leftLeft, leftRight, rightLeft, rightRight) in case matches of
        (Nothing, Nothing, Nothing, Nothing)
            -> case find (\ (left1, right1) -> left1 < left && right1 > right) pairs of
                Nothing -> sort $ pair : pairs
                Just pair1@(left1, right1) -> sort $ (left1, left) : (right, right1) : delete pair1 pairs
        (Just pair1@(_, right1), Nothing, Nothing, Nothing) -> sort $ (right, right1) : delete pair1 pairs
        (Nothing, Just pair1@(left1, _), Nothing, Nothing) -> sort $ (left1, right) : delete pair1 pairs
        (Nothing, Nothing, Just pair1@(_, right1), Nothing) -> sort $ (left, right1) : delete pair1 pairs
        (Nothing, Nothing, Nothing, Just pair1@(left1, _)) -> sort $ (left1, left) : delete pair1 pairs
        (Just pair1, Nothing, Nothing, Just pair2) | pair1 == pair && pair2 == pair -> delete pair pairs
        (Nothing, Just pair1@(left1, _), Just pair2@(_, right2), Nothing)
            -> sort $ (left1, right2) : delete pair2 (delete pair1 pairs)
        _ -> error $ "Unhandled matches: " ++ show matches
    where
        leftLeft = search left fst
        leftRight = search left snd
        rightLeft = search right fst
        rightRight = search right snd
        search side element = find ((==side) . element) pairs

rangeLength :: [ColumnPair] -> Int
rangeLength xs = sum [ right - left + 1 | (left, right) <- xs ]

union :: [ColumnPair] -> [ColumnPair] -> [ColumnPair]
union ranges1 ranges2 = sort
                      . asColumnPairs
                      . Range.joinRanges
                      $ Range.union (asRanges ranges1) (asRanges ranges2)
    where
        asRanges = map $ uncurry (+=+)
        asColumnPairs = map bounds

parse :: [String] -> [Row]
parse = map combineGroup
      . groupBy (\ a b -> fst a == fst b)
      . sort
      . snd
      . foldl combineParse ((0, 0), [])

combineGroup :: [(Int, ColumnPair)] -> Row
combineGroup [] = error $ "Empty group"
combineGroup xs@((row, _):_) = (row, map snd xs)

combineParse :: (Position, [(Int, ColumnPair)]) -> String -> (Position, [(Int, ColumnPair)])
combineParse ((row, column), ranges) x
    = case words x of
        ["U", steps, _] -> ((row - read steps, column), ranges)
        ["D", steps, _] -> ((row + read steps, column), ranges)
        ["L", steps, _] ->
            let newColumn = column - read steps in
                ((row, newColumn), ranges ++ [(row, (newColumn, column))])
        ["R", steps, _] ->
            let newColumn = column + read steps in
                ((row, newColumn), ranges ++[(row, (column, newColumn))])
        _ -> error $ "Cannot parse " ++ x

bounds :: (Enum a, Ord a, Show a) => Range a -> (a, a)
bounds (SingletonRange x) = (x, x)
bounds spanRange@(SpanRange _ _)
    = case [spanRange] `Range.union` [] of -- Use union to turn something like 5 *=+ 1 into 1 +=* 5
         [SpanRange left right] -> (getBound succ left, getBound pred right)
         _ -> error $ "Unhandled span range: " ++ show spanRange
bounds r = error $ "Finding bounds is not supported for " ++ show r

getBound :: (a -> a) -> Bound a -> a
getBound _ (Bound x Inclusive) = x
getBound f (Bound x Exclusive) = f x