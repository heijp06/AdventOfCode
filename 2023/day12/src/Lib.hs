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

data MatchState = MatchState { matches :: Int
                             , state :: Map.Map Record Int
                             } deriving Show

part1 :: [String] -> Int
part1 = sum . map (match . parse 1)

part2 :: [String] -> Int
part2 = sum . map (match . parse 5)

match :: Record -> Int
match record = matches . head . dropWhile (not . Map.null . state) $ iterate doMatch (MatchState 0 (Map.singleton record 1))

doMatch :: MatchState -> MatchState
doMatch MatchState{..} = foldr combineMatchState (MatchState matches Map.empty) (Map.toList state)

combineMatchState :: (Record, Int) -> MatchState -> MatchState
combineMatchState ((xs, []), n) MatchState{..} | '#' `notElem` xs = MatchState { matches = matches + n, .. }
combineMatchState ((_, []), _) matchState = matchState
combineMatchState (([], _), _) matchState = matchState
combineMatchState ((xs, is), _) matchState | length xs < sum is + length is - 1 = matchState
combineMatchState (('.':xs, is), n) MatchState{..} = MatchState { state = Map.insertWith (+) (xs, is) n state, .. }
combineMatchState ((xs@('#':_), i:_), _) matchState | '.' `elem` take i xs = matchState
combineMatchState ((xs@('#':_), [i]), n) MatchState{..} | length xs == i = MatchState { matches = matches + n, .. }
combineMatchState ((xs@('#':_), i:_), _) matchState | xs !! i == '#' = matchState
combineMatchState ((xs@('#':_), i:js), n) MatchState{..} =
    MatchState { state = Map.insertWith (+) (drop (i + 1) xs, js) n state, .. }
combineMatchState ((xs@('?':ys), is@(i:_)), n) MatchState{..} | '.' `elem` take i xs =
    MatchState { state = Map.insertWith (+) (ys, is) n state, .. }
combineMatchState ((xs@('?':_), [i]), n) MatchState{..} | length xs == i = MatchState { matches = matches + n, .. }
combineMatchState ((xs@('?':ys), is@(i:_)), n) MatchState{..} | xs !! i == '#' =
    MatchState { state = Map.insertWith (+) (ys, is) n state, .. }
combineMatchState ((xs@('?':ys), is@(i:js)), n) MatchState{..} = MatchState { state = state2, .. }
    where
        state1 = Map.insertWith (+) (ys, is) n state
        state2 = Map.insertWith (+) (drop (i + 1) xs, js) n state1
combineMatchState record matchState = error $ printf "Unhandled case: '%s' %s" (show record) (show matchState)

parse :: Int -> String -> (String, [Int])
parse n xs = case words xs of
                    [condition, sizes] ->
                        ( intercalate "?" $ replicate n condition
                        , read $ printf "[%s]" (intercalate "," $ replicate n sizes)
                        )
                    _ -> error $ "Cannot parse: " ++ xs