module Modules ( Module
               , State
               , parse
               ) where

import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data State = Off | On deriving Show

data Module = Button [String]
            | Broadcaster [String]
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
    return (xs, m)

broadcaster :: ReadP (String, Module)
broadcaster = undefined

flipFlop :: ReadP (String, Module)
flipFlop = undefined

conjunction :: ReadP (String, Module)
conjunction = undefined