import Test.Hspec hiding (example)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import qualified Data.Map as Map

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "parse" $ do
        it "parse" $ parse exampleParse `shouldBe` ( Map.fromList [ ((0, 0), ((0, -1), (0, 1)))
                                                                  , ((1, 0), ((-1, 0), (1, 0)))
                                                                  , ((2, 0), ((0, 1), (1, 0)))
                                                                  , ((3, 0), ((-1, 0), (0, 1)))
                                                                  , ((0, 1), ((-1, 0), (0, -1)))
                                                                  , ((2, 1), ((0, -1), (1, 0)))
                                                                  , ((3, 1), ((0, -1), (-1, 0)))
                                                                  ], (3, 1) )

    describe "reachable" $ do
        it "reachable example1 N" $ uncurry reachable (parse example1) (0, -1) `shouldBe` False
        it "reachable example1 E" $ uncurry reachable (parse example1) (1, 0) `shouldBe` True
        it "reachable example1 S" $ uncurry reachable (parse example1) (0, 1) `shouldBe` True
        it "reachable example1 W" $ uncurry reachable (parse example1) (-1, 0) `shouldBe` False
        it "reachable example1 (1, 2) (1, 0)" $ let (grid, _) = parse example1
                                                in reachable grid (1, 2) (1, 0) `shouldBe` False

    describe "directionsAfterStart" $ do
        it "directionsAfterStart example1" $ uncurry directionsAfterStart (parse example1) `shouldBe` [ (1, 0), (0, 1) ]
        it "directionsAfterStart example2" $ uncurry directionsAfterStart (parse example2) `shouldBe` [ (1, 0), (0, 1) ]
    
    describe "next" $ do
        it "next example1 start" $ let (grid, start) = parse example1
                                   in next grid (start, (2, 1)) `shouldBe` ((1, 2), start)
        it "next example1 start" $ let (grid, start) = parse example1
                                   in next grid ((1, 2), start) `shouldBe` ((1, 3), (1, 2))

    describe "Part 1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` result1
        it "part1 example2" $ part1 example2 `shouldBe` result2

exampleParse :: [String]
exampleParse = [ "|-F7"
               , "J.LS"
               ]

example1 :: [String]
example1 = [ "-L|F7"
           , "7S-7|"
           , "L|7||"
           , "-L-J|"
           , "L|-JF"
           ]

result1 :: Int
result1 = 4

example2 :: [String]
example2 = [ "7-F7-"
           , ".FJ|7"
           , "SJLL7"
           , "|F--J"
           , "LJ.LJ"
           ]

result2 :: Int
result2 = 8
