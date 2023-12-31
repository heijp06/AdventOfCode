import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` result1

    describe "part2" $ do
        it "part2 example2" $ part2 example2 `shouldBe` result2
        it "part2 exampleA" $ part2 exampleA `shouldBe` resultA
        it "part2 exampleB" $ part2 exampleB `shouldBe` resultB
        it "part2 exampleC" $ part2 exampleC `shouldBe` resultC
        it "part2 exampleD" $ part2 exampleD `shouldBe` resultD
        it "part2 reverse exampleA" $ part2 (reverse exampleA) `shouldBe` resultA
        it "part2 reverse exampleB" $ part2 (reverse exampleB) `shouldBe` resultB
        it "part2 reverse exampleC" $ part2 (reverse exampleC) `shouldBe` resultC
        it "part2 reverse exampleD" $ part2 (reverse exampleD) `shouldBe` resultD
