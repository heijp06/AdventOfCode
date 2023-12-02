import Test.Hspec hiding (example)

import Lib
import Game

example :: [String]
example = [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
          , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
          , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
          , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
          , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
          ]

main :: IO ()
main = hspec $ do
    describe "Parse" $ do
        it "parse -> head example" $ parse (head example) `shouldBe` Game { identifier = 1
                                                                          , draws = [ Draw { red = 4, green = 0, blue = 3 }
                                                                                    , Draw { red = 1, green = 2, blue = 6 }
                                                                                    , Draw { red = 0, green = 2, blue = 0 }
                                                                                    ]
                                                                          }

    describe "Part 1" $ do
        it "part1" $ part1 example `shouldBe` 8

    describe "Part 2" $ do
        it "part2" $ part2 example `shouldBe` 2286

