{-# LANGUAGE RecordWildCards, TupleSections #-}

module Lib
    ( parse
    , part1
    , part2
    , path
    ) where

import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph = Map.Map String [String]
type Connection = Set.Set String

data Path = Path { front :: String
                 , trail :: [String]
                 } deriving Show

instance Eq Path where
    path1 == path2 = front path1 == front path2

instance Ord Path where
    path1 `compare` path2 = front path1 `compare` front path2

data PathState = PathState { seen :: Set.Set Path
                           , current :: Set.Set Path
                           }

data CountState = CountState { number :: Int
                             , excluded :: Set.Set Connection
                             , continue :: Bool
                             }

part1 :: [String] -> Int
part1 xs = (count + 1) * (length graph - count - 1)
    where
        graph = parse xs
        start = fst . head $ Map.toList graph
        count = length
              . filter ((>3) . numberOfPaths graph start)
              . filter (/=start)
              $ Map.keys graph

part2 :: [String] -> Int
part2 = undefined

numberOfPaths :: Graph -> String -> String -> Int
numberOfPaths graph start end = number
                              . head
                              . dropWhile continue
                              . iterate (doCount graph start end)
                              $ CountState 0 Set.empty True

doCount :: Graph -> String -> String -> CountState -> CountState
doCount graph start end CountState{..} =
    case path graph start end excluded of
        [] -> CountState { continue = False, .. }
        xs -> CountState { number = number + 1
                         , excluded = Set.union excluded (pathToExcluded xs)
                         , continue = number < 3
                         }

pathToExcluded :: [String] -> Set.Set Connection
pathToExcluded xs = Set.fromList
                  $ zipWith (\ a b -> Set.fromList [ a, b ]) xs (tail xs)

path :: Graph -> String -> String -> Set.Set Connection -> [String]
path graph start end excluded =
    if Set.null paths
        then []
        else trail . head $ Set.toList paths
    where
        pathState = head
                  . dropWhile (not . Set.null . current)
                  . iterate (step graph excluded end)
                  . PathState Set.empty
                  . Set.singleton 
                  $ Path start [start]
        paths = Set.filter (Path end [] ==) $ seen pathState

step :: Graph -> Set.Set Connection -> String -> PathState -> PathState
step graph excluded end PathState{..} =
    if Set.null new || Path end [] `Set.member` both
        then PathState both Set.empty
        else PathState both new
    where
        both = Set.union seen current
        new = Set.fromList [ p'
                           | p <- Set.toList current
                           , n <- graph ! front p
                           , let p' = Path n (n : trail p)
                           , Set.fromList [ n, front p ] `Set.notMember` excluded
                           , p' `Set.notMember` both ]

parse :: [String] -> Graph
parse = foldr combineLine Map.empty

combineLine :: String -> Graph -> Graph
combineLine xs graph = foldr addConnection graph $ parseLine xs

addConnection :: (String, [String]) -> Graph -> Graph
addConnection = uncurry $ Map.insertWith (++)

parseLine :: String -> [(String, [String])]
parseLine xs = map ((first,) . (:[])) rest ++ map (,[first]) rest
    where
        first = init . head $ words xs
        rest = tail $ words xs
