module Lib
    ( damaged
    , inside
    , left
    , operational
    , outside
    , part1
    , part2
    , partitions
    , right
    ) where

import Data.List (intercalate)
import Text.Printf (printf)

-- import Debug.Trace (trace)

part1 :: [String] -> Int
part1 = sum . map (arrangements . parse' 1)

part2 :: [String] -> Int
part2 = sum . map (arrangements . parse' 5)

arrangements :: (String, [Int]) -> Int
arrangements record = foldr (combine record) 0 [inside, outside, left, right]

combine :: (String, [Int]) -> ((String, [Int]) -> Int) -> Int -> Int
combine record f n = f record + n

inside :: (String, [Int]) -> Int
inside (record, sizes) = sum [ 1 | p <- ps, arrangement p sizes `fits` record ]
    where
        ps = partitions (length record - sum sizes) (length sizes - 1)

outside :: (String, [Int]) -> Int
outside (record, sizes) = sum [ 1 | p <- ps
                              , (operational (head p) ++ arrangement (tail $ tail p) sizes ++ operational (head (tail p)))
                                        `fits` record
                              ]
    where
        ps = partitions (length record - sum sizes) (length sizes + 1)

left :: (String, [Int]) -> Int
left (record, sizes) = sum [ 1 | p <- ps
                           , (operational (head p) ++ arrangement (tail p) sizes) `fits` record ]
    where
        ps = partitions (length record - sum sizes) (length sizes)

right :: (String, [Int]) -> Int
right (record, sizes) = sum [ 1 | p <- ps
                            , (arrangement (tail p) sizes ++ operational (head p)) `fits` record ]
    where
        ps = partitions (length record - sum sizes) (length sizes)

arrangement :: [Int] -> [Int] -> String
-- arrangement ops dams | trace (printf "%s %s" (show ops) (show dams)) False = undefined
arrangement [] _ = error "ops cannot be empty"
arrangement _ [] = error "dams cannot be empty"
arrangement ops (dam:dams) = damaged dam ++ concat (zipWith make ops dams)

make :: Int -> Int -> String
make op dam = operational op ++ damaged dam

fits :: String -> String -> Bool
-- fits record arr | trace (printf "%s %s" record arr) False = undefined
fits record arr = ('.', '#') `notElem` zs && ('#', '.') `notElem` zs
    where
        zs = zip record arr

operational :: Int -> String
operational n = replicate n '.'

damaged :: Int -> String
damaged n = replicate n '#'

partitions :: Int -> Int -> [[Int]]
-- partitions n len | trace (printf "%d %d" n len) False = undefined
partitions n _ | n <= 0 = []
partitions _ len | len <= 0 = []
partitions n 1 = [[n]]
partitions n len = [ n - i : p | i <- [ 1 .. n - 1 ], p <- partitions i (len - 1) ]

parse' :: Int -> String -> (String, [Int])
parse' n xs = case words xs of
                    [condition, sizes] ->
                        ( intercalate "?" $ replicate n condition
                        , read $ printf "[%s]" (intercalate "," $ replicate n sizes)
                        )
                    _ -> error $ "Cannot parse: " ++ xs