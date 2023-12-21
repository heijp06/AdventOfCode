import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` result1

    describe "part2" $ do
        it "part2 exampleA" $ part2 exampleA `shouldBe` resultA
        it "part2 exampleB" $ part2 exampleB `shouldBe` resultB
        it "part2 example2" $ part2 example2 `shouldBe` result2

