import Test.Hspec hiding (example)

import Lib

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "parse example" $ parse example `shouldBe` mapping

    describe "mapRanges" $ do
        it "mapRanges seed -> soil" $ [ mapRanges s (seedToSoil mapping) | s <- [ 79, 14, 55, 13] ] `shouldBe` [ 81, 14, 57, 13 ]

    describe "mapCategories" $ do
        it "mapCategories mapping" $ [ mapCategories s mapping | s <- [ 79, 14, 55, 13 ] ] `shouldBe` [ 82, 43, 86, 35 ]
    
    describe "seeds" $ do
        it "seeds example" $ seeds (head example) `shouldBe` [ 79, 14, 55, 13 ] 
    
    describe "seedRanges" $ do
        it "seedRanges example" $ seedRanges (head example) `shouldBe` [ (79, 14), (55, 13) ]
    
    describe "splitRange" $ do
        it "splitRange" $ map (splitRange (Range 40 20 5))
            [ (0, 10) , (18, 10) , (13, 10) , (21, 2) , (21, 10) , (30, 10) ] `shouldBe`
            [ ([ (0, 10) ], []), ([ (18, 2), (25, 3) ], [ (40, 5) ]), ([ (13, 7) ], [ (40, 3) ]), ([], [ (41, 2) ]), ([ (25, 6) ], [ (41, 4) ]), ([ (30, 10) ], []) ]

    describe "splitRanges" $ do
        it "splitRanges" $ splitRanges (seedToSoil mapping) [(79, 14), (55, 13)] `shouldBe` [(81, 14), (57, 13)]
        it "splitRanges [(49, 2), (97, 4)]" $ splitRanges (seedToSoil mapping) [(49, 2), (97, 4)] `shouldBe` [(49, 1), (100, 1), (52, 1), (99, 1), (50, 2)]

    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 35

    describe "Part 2" $ do
        it "part2 example" $ part2 example `shouldBe` 46

example :: [String]
example = [ "seeds: 79 14 55 13"
          , ""
          , "seed-to-soil map:"
          , "50 98 2"
          , "52 50 48"
          , ""
          , "soil-to-fertilizer map:"
          , "0 15 37"
          , "37 52 2"
          , "39 0 15"
          , ""
          , "fertilizer-to-water map:"
          , "49 53 8"
          , "0 11 42"
          , "42 0 7"
          , "57 7 4"
          , ""
          , "water-to-light map:"
          , "88 18 7"
          , "18 25 70"
          , ""
          , "light-to-temperature map:"
          , "45 77 23"
          , "81 45 19"
          , "68 64 13"
          , ""
          , "temperature-to-humidity map:"
          , "0 69 1"
          , "1 0 69"
          , ""
          , "humidity-to-location map:"
          , "60 56 37"
          , "56 93 4"
          ]

mapping :: Mapping
mapping = Mapping { seedToSoil = [ Range 50 98 2 , Range 52 50 48 ]
                  , soilToFertilizer = [ Range 0 15 37 , Range 37 52 2 , Range 39 0 15 ]
                  , fertilizerToWater = [ Range 49 53 8 , Range 0 11 42 , Range 42 0 7 , Range 57 7 4 ]
                  , waterToLight = [ Range 88 18 7 , Range 18 25 70 ]
                  , lightToTemperature = [ Range 45 77 23 , Range 81 45 19 , Range 68 64 13 ]
                  , temperatureToHumidity = [ Range 0 69 1 , Range 1 0 69 ]
                  , humidityToLocation = [ Range 60 56 37 , Range 56 93 4 ]
                  }
