{-# LANGUAGE RecordWildCards #-}

module Lib
    ( addRatings
    , applyWorkflow
    , parse
    , part1
    , part2
    , sortPart
    ) where

import Data.Map ((!))
import qualified Data.Map as Map

import Parts

data Result = Accepted | Rejected deriving (Eq, Show)
type Workflows = Map.Map String [Rule]

part1 :: [String] -> Int
part1 xs = sum [ addRatings part | part <- parts, sortPart workflows "in" part == Accepted ]
    where
        (parts, workflows) = parse xs

part2 :: [String] -> Int
part2 = undefined

addRatings :: Part -> Int
addRatings Part{..} = x + m + a +s

sortPart :: Workflows -> String -> Part -> Result
sortPart workflows name part =
    case applyWorkflow part (workflows ! name) of
        "A" -> Accepted
        "R" -> Rejected
        xs -> sortPart workflows xs part

applyWorkflow :: Part -> [Rule] -> String
applyWorkflow part = foldr (combineRules part) $ error "No rules applies"

combineRules :: Part -> Rule -> String -> String
combineRules part (f, xs) acc = if f part then xs else acc

parse :: [String] -> ([Part], Workflows)
parse = foldr combineParse ([], Map.empty)

combineParse :: String -> ([Part], Workflows) -> ([Part], Workflows)
combineParse [] acc = acc
combineParse xs@('{':_) (parts, rules)  = (parsePart xs : parts, rules)
combineParse xs (parts, rules) = (parts, uncurry Map.insert (parseWorkflow xs) rules)