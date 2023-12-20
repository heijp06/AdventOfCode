module Examples ( example1a
                , example1b
                , example2
                , result1a
                , result1b
                , result2
                ) where

example1a :: [String]
example1a = [ "broadcaster -> a, b, c"
            , "%a -> b"
            , "%b -> c"
            , "%c -> inv"
            , "&inv -> a"
            ]

result1a :: Int
result1a = 32000000

example1b :: [String]
example1b = [ "broadcaster -> a"
            , "%a -> inv, con"
            , "&inv -> b"
            , "%b -> con"
            , "&con -> output"
            ]

result1b :: Int
result1b = 11687500

example2 :: [String]
example2 = example1a

result2 :: Int
result2 = -1

