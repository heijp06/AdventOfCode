module Game ( Draw(..)
            , Game(..)
            , parse
            ) where

import Data.Char (isDigit, isLower)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data Game = Game { identifier :: Int
                 , draws :: [Draw]
                 } deriving (Eq, Show)

data Draw = Draw { red :: Int
                 , green :: Int
                 , blue :: Int
                 } deriving (Eq, Show)

parse :: String -> Game
parse = doParse game

doParse :: Show a => ReadP a -> String -> a
doParse parser input =
    case readP_to_S parser input of
        [(result, [])] -> result
        x -> error $ show x

game :: ReadP Game
game = do
    i <- parseIdentifier
    ds <- parseDraws
    eof
    return Game { identifier = i, draws = ds }

parseIdentifier :: ReadP Int
parseIdentifier = do
    _ <- string "Game "
    n <- number
    _ <- string ": "
    return n

number :: ReadP Int
number = read <$> many1 digit

digit :: ReadP Char
digit = satisfy isDigit

parseDraws :: ReadP [Draw]
parseDraws = sepBy1 parseDraw (string "; ")

parseDraw :: ReadP Draw
parseDraw = do
    cubes <- Map.fromList <$> sepBy1 parseCube (string ", ")
    return Draw { red = numberOf "red" cubes
                , green = numberOf "green" cubes
                , blue = numberOf "blue" cubes
                }
        where
            numberOf = Map.findWithDefault 0

parseCube :: ReadP (String, Int)
parseCube = do
    n <- number
    skipSpaces
    cs <- many1 letter
    return (cs, n)

letter :: ReadP Char
letter = satisfy isLower