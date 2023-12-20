import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part1" $ do
        it "part1 example1a" $ part1 example1a `shouldBe` result1a
        it "part1 example1b" $ part1 example1b `shouldBe` result1b

    describe "part2" $ do
        it "part2 example2" $ part2 example2 `shouldBe` result2

