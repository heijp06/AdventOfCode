import Test.Hspec hiding (example)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib

example :: [String]
example = [ "#.##..##."
          , "..#.##.#."
          , "##......#"
          , "##......#"
          , "..#.##.#."
          , "..##..##."
          , "#.#.##.#."
          , ""
          , "#...##..#"
          , "#....#..#"
          , "..##..###"
          , "#####.##."
          , "#####.##."
          , "..##..###"
          , "#....#..#"
          ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} $ do
    describe "part1" $ do
        it "part1 example" $ part1 example `shouldBe` 405

