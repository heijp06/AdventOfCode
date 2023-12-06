import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "Time:      7  15   30"
          , "Distance:  9  40  200"
          ]

main :: IO ()
main = hspec $ do
    describe "numbers" $ do
        it "numbers head example" $ numbers (head example) `shouldBe` [ 7, 15, 30 ]

    describe "parse" $ do
        it "parse example" $ parse example `shouldBe` [ (7, 9), (15, 40), (30, 200) ]

    describe "count" $ do
        it "count 7 9" $ count 7 9 `shouldBe` 4
        it "count 15 40" $ count 15 40 `shouldBe` 8
        it "count 30 200" $ count 30 200 `shouldBe` 9

    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 288

