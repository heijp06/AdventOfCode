module Parts ( Part(..)
             , Rule(..)
             , allParts
             , parseWorkflow
             , parsePart
             ) where

import Data.Char (isDigit, isLower)
import Data.Range (Range(..), (+=+), union)
import Text.ParserCombinators.ReadP

data Part = Part { x :: [Range Int]
                 , m :: [Range Int]
                 , a :: [Range Int]
                 , s :: [Range Int]
                 } deriving (Show)

instance Semigroup Part where
    left <> right = Part { x = x left `union` x right
                         , m = m left `union` m right
                         , a = a left `union` a right
                         , s = s left `union` s right
                         }

instance Monoid Part where
    mempty = Part [] [] [] []

data Rule = Rule { for :: Char
                 , range :: Range Int
                 , accessor :: Part -> [Range Int]
                 , target :: String
                 }

allParts :: Part
allParts = Part [1 +=+ 4000] [1 +=+ 4000] [1 +=+ 4000] [1 +=+ 4000]

parse :: ReadP a -> String -> a
parse parser input =
    case readP_to_S parser input of
        [(result, [])] -> result
        _ -> error "Parse failed"

parsePart :: String -> Part
parsePart = parse part

part :: ReadP Part
part = do
    _ <- char '{'
    categoryX <- category 'x'
    _ <- char ','
    categoryM <- category 'm'
    _ <- char ','
    categoryA <- category 'a'
    _ <- char ','
    categoryS <- category 's'
    _ <- char '}'
    return $ Part categoryX categoryM categoryA categoryS

category :: Char -> ReadP [Range Int]
category c = do
    _ <- char c
    _ <- char '='
    n <- integer
    return [SingletonRange n]

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
    t <- target'
    case (name, comp) of
        ('x', '<') -> return $ Rule 'x' (1 +=+ (n-1)) x t
        ('x', '>') -> return $ Rule 'x' ((n+1) +=+ 4000) x t
        ('m', '<') -> return $ Rule 'm' (1 +=+ (n-1)) m t
        ('m', '>') -> return $ Rule 'm' ((n+1) +=+ 4000) m t
        ('a', '<') -> return $ Rule 'a' (1 +=+ (n-1)) a t
        ('a', '>') -> return $ Rule 'a' ((n+1) +=+ 4000) a t
        ('s', '<') -> return $ Rule 's' (1 +=+ (n-1)) s t
        ('s', '>') -> return $ Rule 's' ((n+1) +=+ 4000) s t
        _ -> error $ "Did not expect: " ++ show (name, comp)

lastRule :: ReadP Rule
lastRule = do Rule '?' (1 +=+ 4000) (const [1 +=+ 4000]) <$> target'

target' :: ReadP String
target' = many1 (satisfy isLower) +++ ((:[]) <$> satisfy (`elem` "AR"))

integer :: ReadP Int
integer = do
    xs <- many1 $ satisfy isDigit
    return $ read xs