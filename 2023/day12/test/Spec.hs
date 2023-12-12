import Data.MultiSet as M
import Test.Hspec hiding (example)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib

example :: [String]
example = [ "???.### 1,1,3"
          , ".??..??...?##. 1,1,3"
          , "?#?#?#?#?#?#?#? 1,3,1,6"
          , "????.#...#... 4,1,1"
          , "????.######..#####. 1,6,5"
          , "?###???????? 3,2,1"
          ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "partitions" $ do
        it "partitions 5 3" $ M.fromList (partitions 5 3) `shouldBe`
            M.fromList [ [1, 1, 3], [1, 2, 2], [1, 3, 1], [2, 1, 2], [2, 2, 1], [3, 1, 1] ]
    
    describe "inside, outside, left, right" $ do
        it "inside" $ inside ("??????", [1, 1, 1]) `shouldBe` 2
        it "outside" $ outside ("??????", [1, 1]) `shouldBe` 3
        it "left" $ left ("????#?", [1, 1]) `shouldBe` 0
        it "left" $ left ("??????", [1, 1]) `shouldBe` 3
        it "right" $ right ("?#????", [1, 1]) `shouldBe` 0
        it "right" $ right ("??????", [1, 1]) `shouldBe` 3

    describe "part1" $ do
        it "part1 example" $ part1 example `shouldBe` 21

    describe "part2" $ do
        it "part2 example" $ part2 example `shouldBe` 525152

