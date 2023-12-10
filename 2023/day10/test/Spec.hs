{-# LANGUAGE TupleSections #-}

import Test.Hspec hiding (example)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import qualified Data.Map as Map
import qualified Data.Set as Set

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

    describe "loop" $ do
        it "loop example1" $ Set.fromList (loop example1) `shouldBe`
                                Set.fromList [(1, 1), (2, 1), (3, 1), (3, 2), (3, 3), (2, 3), (1, 3), (1, 2)]

    describe "Part 1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` result1
        it "part1 example2" $ part1 example2 `shouldBe` result2

    describe "Part 1" $ do
        it "part1 example1" $ part1 example1 `shouldBe` result1
        it "part1 example2" $ part1 example2 `shouldBe` result2

    describe "double" $ do
        it "double example1" $ Set.fromList (double $ loop example1) `shouldBe`
                                Set.fromList [ (2, 2), (3, 2), (4, 2), (5, 2), (6, 2), (6, 3), (6, 4), (6, 5)
                                             , (6, 6), (5, 6), (4, 6), (3, 6), (2, 6), (2, 5), (2, 4), (2, 3)]

    describe "border" $ do
        it "border example1" $ border (1, 1) (3, 3) `shouldBe`
            Set.unions [ Set.fromList $ map (0,) [0..4]
                       , Set.fromList $ map (4,) [0..4]
                       , Set.fromList $ map (,0) [0..4]
                       , Set.fromList $ map (,4) [0..4]
                       ]

    describe "Part 2" $ do
        it "part2 example1" $ part2 example1 `shouldBe` 1
        it "part2 example2" $ part2 example2 `shouldBe` 1
        it "part2 example2A" $ part2 example2A `shouldBe` result2A
        it "part2 example2B" $ part2 example2B `shouldBe` result2B
        it "part2 example2C" $ part2 example2C `shouldBe` result2C
        it "part2 example2D" $ part2 example2D `shouldBe` result2D

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

example2A :: [String]
example2A = [ "..........."
            , ".S-------7."
            , ".|F-----7|."
            , ".||.....||."
            , ".||.....||."
            , ".|L-7.F-J|."
            , ".|..|.|..|."
            , ".L--J.L--J."
            , "..........."
            ]

result2A :: Int
result2A = 4

example2B :: [String]
example2B = [ ".........."
            , ".S------7."
            , ".|F----7|."
            , ".||....||."
            , ".||....||."
            , ".|L-7F-J|."
            , ".|..||..|."
            , ".L--JL--J."
            , ".........."
            ]

result2B :: Int
result2B = 4

example2C :: [String]
example2C = [ ".F----7F7F7F7F-7...."
            , ".|F--7||||||||FJ...."
            , ".||.FJ||||||||L7...."
            , "FJL7L7LJLJ||LJ.L-7.."
            , "L--J.L7...LJS7F-7L7."
            , "....F-J..F7FJ|L7L7L7"
            , "....L7.F7||L7|.L7L7|"
            , ".....|FJLJ|FJ|F7|.LJ"
            , "....FJL-7.||.||||..."
            , "....L---J.LJ.LJLJ..."
            ]

result2C :: Int
result2C = 8

example2D :: [String]
example2D = [ "FF7FSF7F7F7F7F7F---7"
            , "L|LJ||||||||||||F--J"
            , "FL-7LJLJ||||||LJL-77"
            , "F--JF--7||LJLJ7F7FJ-"
            , "L---JF-JLJ.||-FJLJJ7"
            , "|F|F-JF---7F7-L7L|7|"
            , "|FFJF7L7F-JF7|JL---7"
            , "7-L-JL7||F7|L7F-7F7|"
            , "L.L7LFJ|||||FJL7||LJ"
            , "L7JLJL-JLJLJL--JLJ.L"
            ]

result2D :: Int
result2D = 10