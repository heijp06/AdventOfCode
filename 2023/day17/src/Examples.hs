module Examples ( example1
                , example2
                , example2a
                , example2x2
                , example2x6
                , result1
                , result2
                , result2a
                , result2x2
                , result2x6
                ) where

example1 :: [String]
example1 = [ "2413432311323"
           , "3215453535623"
           , "3255245654254"
           , "3446585845452"
           , "4546657867536"
           , "1438598798454"
           , "4457876987766"
           , "3637877979653"
           , "4654967986887"
           , "4564679986453"
           , "1224686865563"
           , "2546548887735"
           , "4322674655533"
           ]

result1 :: Int
result1 = 102

example2x2 :: [String]
example2x2 = [ "24"
             , "32"
             ]

result2x2 :: Int
result2x2 = 5

example2x6 :: [String]
example2x6 = [ "111111"
             , "999999"
             ]

result2x6 :: Int
result2x6 = 30

example2 :: [String]
example2 = example1

result2 :: Int
result2 = 94

example2a :: [String]
example2a = [ "111111111111"
            , "999999999991"
            , "999999999991"
            , "999999999991"
            , "999999999991"
            ]

result2a :: Int
result2a = 71
