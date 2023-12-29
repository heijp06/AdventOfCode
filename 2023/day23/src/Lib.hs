{-# LANGUAGE RecordWildCards #-}

module Lib
    ( graph
    , parse
    , part1
    , part2
    ) where

import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set

type Position = (Int, Int)
type Direction = (Int, Int)
type Grid = Array.Array Position Char
type Positions = Map.Map Position PathState

type Edge = (Position, Int)
type Graph = Map.Map Position (Set.Set Edge)

data PathState = PathState { steps :: Int
                           , position :: Position
                           , direction :: Direction
                           }

data GraphState = GraphState { weight :: Int 
                             , from :: Position
                             , position2 :: Position
                             , direction2 :: Direction
                             }

part1 :: [String] -> Int
part1 = solve . parse

part2 :: [String] -> Int
part2 xs = fst
         . head
         . dropWhile (not . Map.null . snd)
         $ iterate (step2 end) (0, Map.singleton (start, graph grid) 0)
    where
        grid = parse xs
        (start, end) = endpoints grid

step2 :: Position -> (Int, Map.Map (Position, Graph) Int) -> (Int, Map.Map (Position, Graph) Int)
step2 end (cost, graphs) = foldr (combineStep2 end) (cost, Map.empty) $ Map.toList graphs

combineStep2 :: Position -> ((Position, Graph), Int) -> (Int, Map.Map (Position, Graph) Int) -> (Int, Map.Map (Position, Graph) Int)
combineStep2 end ((current, graph'), currentCost) (cost, graphs) =
    foldr (combineNodes end currentCost (current, graph')) (cost, graphs) $ graph' Map.! current

combineNodes :: Position -> Int -> (Position, Graph) -> (Position, Int) -> (Int, Map.Map (Position, Graph) Int) -> (Int, Map.Map (Position, Graph) Int)
combineNodes end currentCost (current, graph') (node, weight) (cost, graphs) =
    if node == end
        then
            (max cost $ currentCost + weight, graphs)
        else
            (cost, Map.insertWith max (node, removeNode current graph') (currentCost + weight) graphs)

removeNode :: Position -> Graph -> Graph
removeNode node graph' = foldr (removeEdge node) (Map.delete node graph') $ graph' Map.! node

removeEdge :: Position -> (Position, Int) -> Graph -> Graph
removeEdge node (to, weight) graph'
    = Map.insert to (Set.delete (node, weight) (graph' Map.! to)) graph'

graph :: Grid -> Graph
graph grid = fst
           . head
           . dropWhile (not . null . snd)
           $ iterate (buildGraph grid end) (Map.empty, [GraphState 0 start start (1, 0)])
    where
        (start, end) = endpoints grid

buildGraph :: Grid -> Position -> (Graph, [GraphState]) -> (Graph, [GraphState])
buildGraph grid end (graph', states) = foldr (combineGraph grid end) (graph', []) states

combineGraph :: Grid -> Position -> GraphState -> (Graph, [GraphState]) -> (Graph, [GraphState])
combineGraph grid end GraphState{..} (graph', states) =
    case new of
        [] -> error "No further positions found."
        [(p, _)] | p == end -> (addEdge graph' from p (weight + 1), states)
        [(p, d)] -> (graph', GraphState (weight + 1) from p d : states)
        xs -> ( addEdge graph' from position2 weight
              , if position2 `Map.member` graph'
                    then states
                    else map (uncurry $ GraphState 1 position2) xs ++ states)
    where
        new = [ (newPosition, newDirection)
              | newDirection <- [(1, 0), (0, 1), (-1, 0), (0, -1)]
              , let newPosition = position2 `add` newDirection
              , newDirection /= neg direction2
              , Array.inRange (Array.bounds grid) newPosition 
              , grid ! newPosition /= '#'
              ]

addEdge :: Graph -> Position -> Position -> Int -> Graph
addEdge graph' from to weight
    = Map.insertWith Set.union to (Set.singleton (from, weight))
    $ Map.insertWith Set.union from (Set.singleton (to, weight)) graph'

solve :: Grid -> Int
solve grid = steps
           . (Map.! end)
           . fst
           . head
           . dropWhile (not . Map.null . snd)
           $ iterate (step grid) (Map.empty, Map.singleton start $ PathState 0 (1, 0) start)
    where
        (start, end) = endpoints grid

endpoints :: Grid -> (Position, Position)
endpoints grid = (start, end)
    where
        start = fst . head . filter ((=='.') . snd) $ Array.assocs grid
        end = fst . last . filter ((=='.') . snd) $ Array.assocs grid

step :: Grid -> (Positions, Positions) -> (Positions, Positions)
step grid (positions, current) = foldr (combineStep grid) (both, Map.empty) current
    where
        both = Map.unionWith maxSteps positions current

combineStep :: Grid -> PathState -> (Positions, Positions) -> (Positions, Positions)
combineStep grid PathState{..} (positions, current) = (positions, Map.unionWith maxSteps current newPositions)
    where
        newPositions = Map.fromList
                     [ (newPosition, PathState (steps + 1) newPosition newDirection)
                     | (newDirection, symbol) <- [ ((1, 0), 'v'), ((0, 1), '>'), ((-1, 0), '^'), ((0, -1), '<') ]
                     , let newPosition = add position newDirection
                     , newDirection /= neg direction
                     , Array.inRange (Array.bounds grid) newPosition 
                     , grid ! newPosition `elem` [ '.', symbol ]
                     ]

maxSteps :: PathState -> PathState -> PathState
maxSteps state1 state2 = if steps state2 < steps state1 then state1 else state2

add :: Position -> Direction -> Position
add (row, column) (dRow, dColumn) = (row + dRow, column + dColumn)

neg :: Direction -> Direction
neg (dRow, dColumn) = (-dRow, -dColumn)

parse :: [String] -> Grid
parse xs = Array.listArray ((0, 0), (height - 1, width - 1)) $ concat xs
    where
        height = length xs
        width = length $ head xs