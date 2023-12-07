module Lib
    ( Card(..)
    , Hand(..)
    , parse
    , part1
    , part2
    ) where

import Data.List (group, sort)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
                deriving (Eq, Ord, Show)

instance Read Card where
    readsPrec _ "A" = [(Ace, "")]
    readsPrec _ "K" = [(King, "")]
    readsPrec _ "Q" = [(Queen, "")]
    readsPrec _ "J" = [(Jack, "")]
    readsPrec _ "T" = [(Ten, "")]
    readsPrec _ "9" = [(Nine, "")]
    readsPrec _ "8" = [(Eight, "")]
    readsPrec _ "7" = [(Seven, "")]
    readsPrec _ "6" = [(Six, "")]
    readsPrec _ "5" = [(Five, "")]
    readsPrec _ "4" = [(Four, "")]
    readsPrec _ "3" = [(Three, "")]
    readsPrec _ "2" = [(Two, "")]
    readsPrec _ _ = []

data Ord a => Hand a = HighCard a
                     | OnePair a
                     | TwoPair a
                     | ThreeOfAKind a
                     | FullHouse a
                     | FourOfAKind a
                     | FiveOfAKind a deriving (Eq, Ord, Show)

part1 :: [String] -> Int
part1 = sum . zipWith (*) [1..] . map snd . sort . map parse

part2 :: [String] -> Int
part2 = undefined

parse :: String -> (Hand [Card], Int)
parse xs = case words xs of
                [hand, bid] -> (parseHand hand, read bid)
                _ -> error $ "Cannot parse: " ++ xs

parseHand :: String -> Hand [Card]
parseHand xs = case sort . map length . group $ sort cards  of
                [5] -> FiveOfAKind cards
                [1, 4] -> FourOfAKind cards
                [2, 3] -> FullHouse cards
                [1, 1, 3] -> ThreeOfAKind cards
                [1, 2, 2] -> TwoPair cards
                [1, 1, 1, 2] -> OnePair cards
                [1, 1, 1, 1, 1] -> HighCard cards
                _ -> error $ "Cannot parse hand: " ++ xs
    where
        cards = map (read . (:[])) xs :: [Card]
