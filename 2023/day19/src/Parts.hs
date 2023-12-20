module Parts ( Part(..)
             , Rule
             , parseWorkflow
             ) where

import Data.Char (isDigit, isLower)
import Text.ParserCombinators.ReadP

data Part = Part { x :: Int
                 , m :: Int
                 , a :: Int
                 , s :: Int
                 } deriving (Read, Show)

type Rule = (Part -> Bool, String)

parse :: ReadP a -> String -> a
parse parser input =
    case readP_to_S parser input of
        [(result, [])] -> result
        _ -> error "Parse failed"

parseWorkflow :: String -> (String, [Rule])
parseWorkflow = parse workflow

workflow :: ReadP (String, [Rule])
workflow = do
    name <- many1 (satisfy isLower)
    _ <- char '{'
    rules <- sepBy1 rule (char ',')
    _ <- char ','
    lr <- lastRule
    _ <- char '}'
    return (name, rules ++ [lr])
    
rule :: ReadP Rule
rule = do
    name <- satisfy (`elem` "xmas")
    comp <- satisfy (`elem` "<>")
    n <- integer
    _ <- char ':'
    t <- target
    case (name, comp) of
        ('x', '<') -> return ((<n) . x, t)
        ('x', '>') -> return ((>n) . x, t)
        ('m', '<') -> return ((<n) . m, t)
        ('m', '>') -> return ((>n) . m, t)
        ('a', '<') -> return ((<n) . a, t)
        ('a', '>') -> return ((>n) . a, t)
        ('s', '<') -> return ((<n) . s, t)
        ('s', '>') -> return ((>n) . s, t)
        _ -> error $ "Did not expect: " ++ show (name, comp)

lastRule :: ReadP Rule
lastRule = do
    xs <- target
    return (const True, xs)

target :: ReadP String
target = many1 (satisfy isLower) +++ ((:[]) <$> satisfy (`elem` "AR"))

integer :: ReadP Int
integer = do
    xs <- many1 $ satisfy isDigit
    return $ read xs