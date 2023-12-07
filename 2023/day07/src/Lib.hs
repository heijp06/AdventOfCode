module Lib
    ( Card(..)
    , Hand(..)
    , parse
    , part1
    , part2
    ) where

import Data.List (group, sort)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
                deriving (Eq, Ord, Show)

data Ord a => Hand a = HighCard a
                     | OnePair a
                     | TwoPair a
                     | ThreeOfAKind a
                     | FullHouse a
                     | FourOfAKind a
                     | FiveOfAKind a deriving (Eq, Ord, Show)

part1 :: [String] -> Int
part1 = solve id

part2 :: [String] -> Int
part2 = solve replaceJackByJoker

solve :: (Card -> Card) -> [String] -> Int
solve replace = sum . zipWith (*) [1..] . map snd . sort . map (parse replace)

replaceJackByJoker :: Card -> Card
replaceJackByJoker Jack = Joker
replaceJackByJoker c = c

parse :: (Card -> Card) -> String -> (Hand [Card], Int)
parse replace xs = case words xs of
                    [hand, bid] -> (parseHand replace hand, read bid)
                    _ -> error $ "Cannot parse: " ++ xs

parseHand :: (Card -> Card) -> String -> Hand [Card]
parseHand replace xs = case typeOfHand of
                        [5] -> FiveOfAKind cards
                        [1, 4] -> FourOfAKind cards
                        [2, 3] -> FullHouse cards
                        [1, 1, 3] -> ThreeOfAKind cards
                        [1, 2, 2] -> TwoPair cards
                        [1, 1, 1, 2] -> OnePair cards
                        [1, 1, 1, 1, 1] -> HighCard cards
                        _ -> error $ "Cannot parse hand: " ++ xs
    where
        cards = map (replace . parseCard) xs
        typeOfHandWithoutJokers = sort . map length . group . sort $ filter (/=Joker) cards
        jokers = length $ filter (==Joker) cards
        typeOfHand = if jokers == 5 then [5] else init typeOfHandWithoutJokers ++ [last typeOfHandWithoutJokers + jokers]

parseCard :: Char -> Card
parseCard 'A' = Ace
parseCard 'K' = King
parseCard 'Q' = Queen
parseCard 'J' = Jack
parseCard 'T' = Ten
parseCard '9' = Nine
parseCard '8' = Eight
parseCard '7' = Seven
parseCard '6' = Six
parseCard '5' = Five
parseCard '4' = Four
parseCard '3' = Three
parseCard '2' = Two
parseCard c = error $ "Cannot parse card: " ++ [c]