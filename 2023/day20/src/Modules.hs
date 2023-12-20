module Modules ( Module(..)
               , Pulse(..)
               , FlipFlopState(..)
               , parse
               ) where

import Data.Char (isLower)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data FlipFlopState = Off | On deriving Show

data Module = Broadcaster [String]
            | FlipFlop FlipFlopState [String]
            | Conjunction [Pulse] [String]
            | Output deriving Show

data Pulse = Low String | High String deriving Show

doParse :: Show a => ReadP a -> String -> a
doParse parser input =
    case readP_to_S parser input of
        [(result, [])] -> result
        xs -> error $ "Parse failed: " ++ show xs

parse :: [String] -> Map.Map String Module
parse = Map.fromList . setInputs . map (doParse module')

setInputs :: [(String, Module)] -> [(String, Module)]
setInputs xs = map (setInput xs) xs

setInput :: [(String, Module)] -> (String, Module) -> (String, Module)
setInput _ (name, m@(Broadcaster _)) = (name, m)
setInput _ (name, m@(FlipFlop _ _)) = (name, m)
setInput xs (name, Conjunction _ ds) = (name, Conjunction [ Low x | (x, m) <- xs, name `elem` outputs m] ds)

outputs :: Module -> [String]
outputs (Broadcaster os) = os
outputs (FlipFlop _ os) = os
outputs (Conjunction _ os) = os

module' :: ReadP (String, Module)
module' = do
    (xs, m) <- broadcaster <++ flipFlop <++ conjunction
    eof
    return (xs, m)

broadcaster :: ReadP (String, Module)
broadcaster = do
    ns <- string "broadcaster"
    xs <- destinations
    return (ns, Broadcaster xs)

flipFlop :: ReadP (String, Module)
flipFlop = do
    _ <- char '%'
    ns <- name
    xs <- destinations
    return (ns, FlipFlop Off xs)

conjunction :: ReadP (String, Module)
conjunction = do
    _ <- char '&'
    ns <- name
    xs <- destinations
    return (ns, Conjunction [] xs)

destinations :: ReadP [String]
destinations = do
    _ <- string " -> "
    sepBy1 name $ string ", "

name :: ReadP String
name = many1 $ satisfy isLower