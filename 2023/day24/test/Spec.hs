import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import qualified LinearEquations as LE
import Lib
import Examples

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "solve" $ do
        it "solve example1" $ solve (7, 27) example1 `shouldBe` result1

    -- describe "part2" $ do
    --     it "part2 example2" $ part2 example2 `shouldBe` result2

    describe "LinearEquations" $ do
        it "solve" $ LE.solve [ [1, 1]
                              , [3, 2]
                              ] [5, 13] `shouldBe` [3, 2]
        it "solve" $ LE.solve [ [1, 0, 1]
                              , [2, 1, 3]
                              , [1, 1, 7]
                              ] [7, 22, 40] `shouldBe` [2, 3, 5]
