import Test.Hspec hiding (example)

import Lib
import qualified Data.Map as Map

example1 :: [String]
example1 = [ "RL"
           , ""
           , "AAA = (BBB, CCC)"
           , "BBB = (DDD, EEE)"
           , "CCC = (ZZZ, GGG)"
           , "DDD = (DDD, DDD)"
           , "EEE = (EEE, EEE)"
           , "GGG = (GGG, GGG)"
           , "ZZZ = (ZZZ, ZZZ)"
           ]

example2 :: [String]
example2 = [ "LLR"
           , ""
           , "AAA = (BBB, BBB)"
           , "BBB = (AAA, ZZZ)"
           , "ZZZ = (ZZZ, ZZZ)"
           ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "parse example2" $ parse example2 `shouldBe` ("LLR", Map.fromList [ ("AAA", ("BBB", "BBB"))
                                                                             , ("BBB", ("AAA", "ZZZ"))
                                                                             , ("ZZZ", ("ZZZ", "ZZZ"))
                                                                             ])

    describe "Part 1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` 2
        it "part1 example2" $ part1 example2 `shouldBe` 6

