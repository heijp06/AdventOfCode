import Test.Hspec hiding (example)

import Lib

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1" $ 1 `shouldBe` 1
