module Examples ( example1
                , example2
                , result1
                , result2
                ) where

example1 :: [String]
example1 = [ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" ]

result1 :: Int
result1 = 1320

example2 :: [String]
example2 = example1

result2 :: Int
result2 = 145