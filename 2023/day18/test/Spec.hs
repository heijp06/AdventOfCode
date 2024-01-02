import Data.Range
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "bounds'" $ do
        it "SingletonRange 1" $ bounds' (SingletonRange 1) `shouldBe` (1 :: Int, 1)
        it "1 +=+ 5" $ bounds' (1 +=+ 5) `shouldBe` (1 :: Int, 5)
        it "1 *=+ 5" $ bounds' (1 *=+ 5) `shouldBe` (2 :: Int, 5)
        it "1 +=* 5" $ bounds' (1 +=* 5) `shouldBe` (1 :: Int, 4)
        it "1 *=* 5" $ bounds' (1 *=* 5) `shouldBe` (2 :: Int, 4)
        it "5 +=+ 1" $ bounds' (5 +=+ 1) `shouldBe` (1 :: Int, 5)
        it "5 *=+ 1" $ bounds' (5 *=+ 1) `shouldBe` (1 :: Int, 4)
        it "5 +=* 1" $ bounds' (5 +=* 1) `shouldBe` (2 :: Int, 5)
        it "5 *=* 1" $ bounds' (5 *=* 1) `shouldBe` (2 :: Int, 4)

    describe "part1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` result1

    describe "part2" $ do
        it "part2 example2" $ part2 example2 `shouldBe` result2

