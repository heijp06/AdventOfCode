import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part2" $ do
        it "part2 example2" $ part2 example2 `shouldBe` result2

