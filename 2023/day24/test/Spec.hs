import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "solve" $ do
        it "solve example1" $ solve (7, 27) example1 `shouldBe` result1

    describe "part2" $ do
        it "part2 example2" $ part2 example2 `shouldBe` result2

