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
example1 = [ "px{a<2006:qkq,m>2090:A,rfg}"
           , "pv{a>1716:R,A}"
           , "lnx{m>1548:A,A}"
           , "rfg{s<537:gd,x>2440:R,A}"
           , "qs{s>3448:A,lnx}"
           , "qkq{x<1416:A,crn}"
           , "crn{x>2662:A,R}"
           , "in{s<1351:px,qqz}"
           , "qqz{s>2770:qs,m<1801:hdj,R}"
           , "gd{a>3333:R,R}"
           , "hdj{m>838:A,pv}"
           , ""
           , "{x=787,m=2655,a=1222,s=2876}"
           , "{x=1679,m=44,a=2067,s=496}"
           , "{x=2036,m=264,a=79,s=2244}"
           , "{x=2461,m=1339,a=466,s=291}"
           , "{x=2127,m=1623,a=2188,s=1013}"
           ]

result1 :: Int
result1 = 19114

example2 :: [String]
example2 = example1

result2 :: Int
result2 = 167409079868000

exampleA :: [String]
exampleA = [ "in{x<2001:R,A}"
           ]

resultA :: Int
resultA = 2000 * 4000 * 4000 * 4000

exampleB :: [String]
exampleB = [ "in{x<2001:aa,m>1000:bb,R}"
           , "aa{x>1000:A,s>3000:bb,R}"
           , "bb{m<3001:R,A}"]

resultB :: Int
resultB = 1000 * 4000 * 4000 * 4000 -- 1001 +=+ 2000, 1 +=+ 4000, 1 +=+ 4000, 1 +=+ 4000
        + 1000 * 1000 * 4000 * 1000 -- 1 +=+ 1000, 3001 +=+ 4000, 1 +=+ 4000, 3001 +=+ 4000
        + 2000 * 1000 * 4000 * 4000 -- 2001 +=+ 4000, 3001 +=+ 4000, 1 +=+ 4000, 1 +=+ 4000

exampleC :: [String]
exampleC = [ "in{x<2001:aa,R}"
           , "aa{x>1000:A,R}"
           ]

resultC :: Int
resultC = 1000 * 4000 * 4000 * 4000 -- 1001 +=+ 2000, 1 +=+ 4000, 1 +=+ 4000, 1 +=+ 4000