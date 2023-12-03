import Test.Hspec hiding (example)

import qualified Data.Map as Map

import Lib

example :: [String]
example = [ "467..114.."
          , "...*......"
          , "..35..633."
          , "......#..."
          , "617*......"
          , ".....+.58."
          , "..592....."
          , "......755."
          , "...$.*...."
          , ".664.598.."
          ]

connected :: [String]
connected = [ "467......."
            , "...*......"
            , "..35..633."
            , "......#..."
            , "617*......"
            , ".....+...."
            , "..592....."
            , "......755."
            , "...$.*...."
            , ".664.598.."
            ]

exampleGrid :: Grid
exampleGrid = buildGrid example

main :: IO ()
main = hspec $ do
    describe "coordinates" $ do
        it "coordinates 2 3" $ coordinates 2 3 `shouldBe` [ (0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2) ]

    describe "buildGrid" $ do
        it "small grid" $ buildGrid [ "12.", "..#" ] `shouldBe` Map.fromList [ ((0, 0), '1'), ((1, 0), '2'), ((2, 1), '#') ]

    describe "symbols" $ do
        it "symbols" $ symbols exampleGrid `shouldBe` Map.fromList [ ((3, 1), '*')
                                                                           , ((6, 3), '#')
                                                                           , ((3, 4), '*')
                                                                           , ((5, 5) , '+')
                                                                           , ((3, 8), '$')
                                                                           , ((5, 8), '*') 
                                                                           ]
    describe "step" $ do
        it "step symbols" $ step (symbols exampleGrid) exampleGrid `shouldBe` Map.fromList [ ((2, 0), '7')
                                                                                           , ((2, 2), '3')
                                                                                           , ((3, 2), '5')
                                                                                           , ((6, 2), '6')
                                                                                           , ((7, 2), '3')
                                                                                           , ((2, 4), '7')
                                                                                           , ((4, 6), '2')
                                                                                           , ((6, 7), '7')
                                                                                           , ((2, 9), '6')
                                                                                           , ((3, 9), '4')
                                                                                           , ((5, 9), '5')
                                                                                           , ((6, 9), '9')
                                                                                           ]
    describe "allConnected" $ do
        it "allConnected" $ allConnected exampleGrid `shouldBe` buildGrid connected

    describe "Part 1" $ do
        it "part1 example -> 4361" $ part1 example `shouldBe` 4361

