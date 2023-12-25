module Examples ( example1
                , example2
                , result1
                , result2
                ) where

example1 :: [String]
example1 = [ "jqt: rhn xhk nvd"
           , "rsh: frs pzl lsr"
           , "xhk: hfx"
           , "cmg: qnr nvd lhk bvb"
           , "rhn: xhk bvb hfx"
           , "bvb: xhk hfx"
           , "pzl: lsr hfx nvd"
           , "qnr: nvd"
           , "ntq: jqt hfx bvb xhk"
           , "nvd: lhk"
           , "lsr: lhk"
           , "rzs: qnr cmg lsr rsh"
           , "frs: qnr lhk lsr"
           ]

result1 :: Int
result1 = 54

example2 :: [String]
example2 = example1

result2 :: Int
result2 = -1

