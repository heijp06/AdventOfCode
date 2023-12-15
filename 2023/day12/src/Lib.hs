{-# LANGUAGE RecordWildCards #-}

module Lib
    ( MatchState(..)
    , combineMatchState
    , match
    , part1
    , part2
    ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import Text.Printf (printf)

type Record = (String, [Int])

data MatchState = MatchState { count :: Int
                             , partialMatches :: Map.Map Record Int
                             } deriving Show

part1 :: [String] -> Int
part1 = sum . map (match . parse 1)

part2 :: [String] -> Int
part2 = sum . map (match . parse 5)

match :: Record -> Int
match record = count . head . dropWhile (not . Map.null . partialMatches) $ iterate doMatch (MatchState 0 (Map.singleton record 1))

doMatch :: MatchState -> MatchState
doMatch MatchState{..} = foldr combineMatchState (MatchState count Map.empty) (Map.toList partialMatches)

combineMatchState :: (Record, Int) -> MatchState -> MatchState
combineMatchState ((xs, []), n) MatchState{..} | '#' `notElem` xs = MatchState { count = count + n, .. }
combineMatchState ((_, []), _) matchState = matchState
combineMatchState ((xs, is), _) matchState | length xs < sum is + length is - 1 = matchState
combineMatchState (('.':xs, is), n) MatchState{..} = MatchState { partialMatches = Map.insertWith (+) (xs, is) n partialMatches, .. }
combineMatchState ((xs@('#':_), i:_), _) matchState | '.' `elem` take i xs = matchState
combineMatchState ((xs@('?':ys), is@(i:_)), n) MatchState{..} | '.' `elem` take i xs =
    MatchState { partialMatches = Map.insertWith (+) (ys, is) n partialMatches, .. }
combineMatchState ((xs, [i]), n) MatchState{..} | length xs == i = MatchState { count = count + n, .. }
combineMatchState ((xs@('#':_), i:_), _) matchState | xs !! i == '#' = matchState
combineMatchState ((xs@('#':_), i:js), n) MatchState{..} =
    MatchState { partialMatches = Map.insertWith (+) (drop (i + 1) xs, js) n partialMatches, .. }
combineMatchState ((xs@('?':ys), is@(i:_)), n) MatchState{..} | xs !! i == '#' =
    MatchState { partialMatches = Map.insertWith (+) (ys, is) n partialMatches, .. }
combineMatchState ((xs@('?':ys), is@(i:js)), n) MatchState{..} = MatchState { partialMatches = partialMacthes2, .. }
    where
        partialMacthes1 = Map.insertWith (+) (ys, is) n partialMatches
        partialMacthes2 = Map.insertWith (+) (drop (i + 1) xs, js) n partialMacthes1
combineMatchState record matchState = error $ printf "Unhandled case: '%s' %s" (show record) (show matchState)

parse :: Int -> String -> (String, [Int])
parse n xs = case words xs of
                    [condition, sizes] ->
                        ( intercalate "?" $ replicate n condition
                        , read $ printf "[%s]" (intercalate "," $ replicate n sizes)
                        )
                    _ -> error $ "Cannot parse: " ++ xs