module Examples ( example1
                , example2
                , exampleA
                , exampleB
                , result1
                , result2
                , resultA
                , resultB
                ) where

example1 :: [String]
example1 = [ "1,0,1~1,2,1"
           , "0,0,2~2,0,2"
           , "0,2,3~2,2,3"
           , "0,0,4~0,2,4"
           , "2,0,5~2,2,5"
           , "0,1,6~2,1,6"
           , "1,1,8~1,1,9"
           ]

result1 :: Int
result1 = 5

example2 :: [String]
example2 = example1

result2 :: Int
result2 = 7

exampleA :: [String]
exampleA = [ "0,0,1~0,0,1"
           , "0,0,2~0,0,2"
           , "0,0,3~0,0,3"
           , "0,0,4~0,0,4"
           , "0,0,5~0,0,5"
           , "0,0,6~0,0,6"
           , "0,0,7~0,0,7"
           , "0,0,8~0,0,8"
           , "0,0,9~0,0,9"
           , "0,0,10~0,0,10"
           ]

resultA :: Int
resultA = 45

exampleB :: [String]
exampleB = [ "0,0,1~0,0,1"
           , "1,0,1~1,0,1"
           , "0,0,2~1,0,2"
           , "0,0,3~0,0,3"
           , "1,0,3~1,0,3"
           , "0,0,4~1,0,4"
           , "0,0,5~0,0,5"
           , "1,0,5~1,0,5"
           , "0,0,6~1,0,6"
           , "0,0,7~0,0,7"
           , "1,0,7~1,0,7"
           ]

resultB :: Int
resultB = 15