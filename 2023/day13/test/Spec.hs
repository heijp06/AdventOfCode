import Test.Hspec hiding (example)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Examples
import Lib

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part1" $ do
        it "part1 example" $ part1 example `shouldBe` 405

    describe "part2" $ do
        it "part2 example" $ part2 example `shouldBe` 400

