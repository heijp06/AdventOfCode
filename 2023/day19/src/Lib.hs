{-# LANGUAGE RecordWildCards #-}

module Lib
    ( addRatings
    , applyWorkflow
    , filterWorkFlow
    , parse
    , part1
    , part2
    , resultPart2
    , sortPart
    ) where

import Data.Map ((!))
import Data.Range (difference, fromRanges, intersection, rangesOverlap)
import qualified Data.Map as Map

import Parts

data Result = Accepted | Rejected deriving (Eq, Show)
type Workflows = Map.Map String [Rule]

part1 :: [String] -> Int
part1 xs = sum [ addRatings part | part <- parts, sortPart workflows "in" part == Accepted ]
    where
        (parts, workflows) = parse xs

part2 :: [String] -> Int
part2 xs = resultPart2 (result ! "A")
    where
        (_, wf) = parse xs
        stuff = Map.singleton "in" [allParts]
        result = head . dropWhile (any (`notElem` ["A", "R"]) . Map.keys) $ iterate (filterWorkFlow wf) stuff

addRatings :: Part -> Int
addRatings p = sum $ map (\ ys -> l * sum ys `div` length ys) [xs, ms, as, ss]
    where
        xs = fromRanges $ x p
        ms = fromRanges $ m p
        as = fromRanges $ a p
        ss = fromRanges $ s p
        l = product $ map length [xs, ms, as, ss]

resultPart2 :: [Part] -> Int
resultPart2 = sum . map lengthProd

lengthProd :: Part -> Int
lengthProd Part{..} = product $ map (length . fromRanges) [x, m, a, s]

filterWorkFlow :: Workflows -> Map.Map String [Part] -> Map.Map String [Part]
filterWorkFlow workflows = foldr (filterState workflows) Map.empty . Map.toList

filterState :: Workflows -> (String, [Part]) -> Map.Map String [Part] -> Map.Map String [Part]
filterState _ ("A", parts) states = Map.insertWith (++) "A" parts states
filterState _ ("R", parts) states = Map.insertWith (++) "R" parts states
filterState workflows (name, parts) states = foldr (filterParts (workflows ! name)) states parts
-- filterState workflows (name, parts) states = snd $ foldl filterRule (parts, states) (workflows ! name)

filterParts :: [Rule] -> Part -> Map.Map String [Part] -> Map.Map String [Part]
filterParts rules part states = snd $ foldl filterRule (part, states) rules

filterRule :: (Part, Map.Map String [Part]) -> Rule -> (Part, Map.Map String [Part])
filterRule (part@Part{..}, states) Rule{..} = case for of
    'x' -> ( Part { x = x `difference` [range], .. }, Map.insertWith (++) target [Part { x = x `intersection` [range], ..}] states)
    'm' -> ( Part { m = m `difference` [range], .. }, Map.insertWith (++) target [Part { m = m `intersection` [range], ..}] states)
    'a' -> ( Part { a = a `difference` [range], .. }, Map.insertWith (++) target [Part { a = a `intersection` [range], ..}] states)
    's' -> ( Part { s = s `difference` [range], .. }, Map.insertWith (++) target [Part { s = s `intersection` [range], ..}] states)
    '?' -> ( part, Map.insertWith (++) target [part] states)
    _ -> error $ "Unexpected for " ++ show for

sortPart :: Workflows -> String -> Part -> Result
sortPart workflows name part =
    case applyWorkflow part (workflows ! name) of
        "A" -> Accepted
        "R" -> Rejected
        xs -> sortPart workflows xs part

applyWorkflow :: Part -> [Rule] -> String
applyWorkflow part = foldr (combineRules part) $ error "No rules applies"

combineRules :: Part -> Rule -> String -> String
combineRules part Rule{..} acc = if any (rangesOverlap range) (accessor part) then target else acc

parse :: [String] -> ([Part], Workflows)
parse = foldr combineParse ([], Map.empty)

combineParse :: String -> ([Part], Workflows) -> ([Part], Workflows)
combineParse [] acc = acc
combineParse xs@('{':_) (parts, rules)  = (parsePart xs : parts, rules)
combineParse xs (parts, rules) = (parts, uncurry Map.insert (parseWorkflow xs) rules)