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
        it "parse" $ parse id (head example) `shouldBe` (OnePair [ Three, Two, Ten, Three, King ], 765)

    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 6440
        it "part1 jack > three" $ part1 [ "J2222 1", "32222 10" ] `shouldBe` 12

    describe "Part 2" $ do
        it "part2 example" $ part2 example `shouldBe` 5905
        it "part2 joker < two" $ part2 [ "J2222 1", "22222 10" ] `shouldBe` 21
