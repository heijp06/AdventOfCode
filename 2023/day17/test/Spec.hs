import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part1" $ do
        it "part1 example2x2" $ part1 example2x2 `shouldBe` result2x2
        it "part1 example2x6" $ part1 example2x6 `shouldBe` result2x6
        it "part1 example1" $ part1 example1 `shouldBe` result1

    describe "part2" $ do
        it "part2 example2" $ part2 example2 `shouldBe` result2

