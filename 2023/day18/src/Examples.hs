module Examples ( example1
                , example2
                , exampleA
                , exampleB
                , exampleC
                , result1
                , result2
                , resultA
                , resultB
                , resultC
                ) where

example1 :: [String]
example1 = [ "R 6 (#70c710)"
           , "D 5 (#0dc571)"
           , "L 2 (#5713f0)"
           , "D 2 (#d2c081)"
           , "R 2 (#59c680)"
           , "D 2 (#411b91)"
           , "L 5 (#8ceee2)"
           , "U 2 (#caa173)"
           , "L 1 (#1b58a2)"
           , "U 2 (#caa171)"
           , "R 2 (#7807d2)"
           , "U 3 (#a77fa3)"
           , "L 2 (#015232)"
           , "U 2 (#7a21e3)"
           ]

result1 :: Int
result1 = 62

example2 :: [String]
example2 = example1

result2 :: Int
result2 = 952408144115

exampleA :: [String]
exampleA = [ "R 2 _"
           , "D 1 _"
           , "R 2 _"
           , "U 1 _"
           , "R 2 _"
           , "D 3 _"
           , "L 6 _"
           , "U 3 _"
           ]

resultA :: Int
resultA = 27

exampleB :: [String]
exampleB = [ "R 2 _"
           , "D 1 _"
           , "R 1 _"
           , "D 2 _"
           , "L 1 _"
           , "D 1 _"
           , "L 2 _"
           , "U 1 _"
           , "L 1 _"
           , "U 2 _"
           , "R 1 _"
           , "U 1 _"
           ]

resultB :: Int
resultB = 21

exampleC :: [String]
exampleC = [ "R 2 _"
           , "D 2 _"
           , "R 2 _"
           , "U 2 _"
           , "R 6 _"
           , "D 4 _"
           , "L 2 _"
           , "U 2 _"
           , "L 2 _"
           , "D 2 _"
           , "L 6 _"
           , "U 4 _"
           ]

resultC :: Int
resultC = 51