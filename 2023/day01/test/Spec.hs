import Test.Hspec hiding (example)

import Lib

example1 :: [String]
example1 = [ "1abc2"
           , "pqr3stu8vwx"
           , "a1b2c3d4e5f"
           , "treb7uchet"
           ]

example2 :: [String]
example2 = [ "two1nine"
           , "eightwothree"
           , "abcone2threexyz"
           , "xtwone3four"
           , "4nineeightseven2"
           , "zoneight234"
           , "7pqrstsixteen"
           ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example1 -> 142" $ part1 example1 `shouldBe` 142

    describe "Part 2" $ do
        it "part2 example2 -> 281" $ part2 example2 `shouldBe` 281
