import Test.Hspec hiding (example)

import Lib

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "parse example" $ parse example `shouldBe` mapping

    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 35

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
