import Test.Hspec hiding (example)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib

example :: [String]
example = [ "...#......"
          , ".......#.."
          , "#........."
          , ".........."
          , "......#..."
          , ".#........"
          , ".........#"
          , ".........."
          , ".......#.."
          , "#...#....."
          ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "parse" $ do
        it "parse example" $ parse example `shouldBe`
            [(0, 3), (1, 7), (2, 0), (4, 6), (5, 1), (6, 9), (8, 7), (9, 0), (9, 4)]
    
    describe "expand" $ do
        it "expand 2 example" $ expand 2 (parse example) `shouldBe`
            [(0, 4), (1, 9), (2, 0), (5, 8), (6, 1), (7, 12), (10, 9), (11, 0), (11, 5)]

    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 374

