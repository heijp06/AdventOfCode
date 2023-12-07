import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "32T3K 765"
          , "T55J5 684"
          , "KK677 28"
          , "KTJJT 220"
          , "QQQJA 483"
          ]

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "parse" $ parse (head example) `shouldBe` (OnePair [ Three, Two, Ten, Three, King ], 765)

    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 6440
