module Modules ( Module
               , State
               , parse
               ) where

import Data.Char (isLower)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data State = Off | On deriving Show

data Module = Broadcaster [String]
            | FlipFlop State [String]
            | Conjunction [String] deriving Show

doParse :: Show a => ReadP a -> String -> a
doParse parser input =
    case readP_to_S parser input of
        [(result, [])] -> result
        xs -> error $ "Parse failed: " ++ show xs

parse :: [String] -> Map.Map String Module
parse = Map.fromList . map (doParse module')

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
    return (ns, Conjunction xs)

destinations :: ReadP [String]
destinations = do
    _ <- string " -> "
    sepBy1 name $ string ", "

name :: ReadP String
name = many1 $ satisfy isLower