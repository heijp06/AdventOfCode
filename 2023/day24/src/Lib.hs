module Lib
    ( matrix
    , parse
    , part1
    , part2
    , solve
    ) where

import Data.List (tails)
import Text.Printf (printf)

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Stone = (Position, Velocity)

part1 :: [String] -> Int
part1 = solve (200000000000000, 400000000000000)

part2 :: [String] -> [String]
part2 xs = xyuv ++ [""] ++ zw
    where
        xyuv = map equationXyuv $ matrix xs
        zw = map (equationZw . parse) $ take 2 xs

equationXyuv :: ([Int], Int) -> String
equationXyuv (coefficients, result) = lhs ++ "=" ++ show result
    where
        lhs = concat $ zipWith term "xyuv" coefficients

equationZw :: (Position, Velocity) -> String
equationZw ((xn, _, zn), (un, _, wn))
    = printf "(%d-x)w-(%d-u)z=(%d-x)(%d)-(%d-u)(%d)" xn un xn wn un zn

term :: Char -> Int -> String
term _ 0 = ""
term name 1 = '+' : [name]
term name (-1) = '-' : [name]
term name coefficient | coefficient < 0 = show coefficient ++ [name]
term name coefficient = '+' : show coefficient ++ [name]

matrix :: [String] -> [([Int], Int)]
matrix xs = foldr (addRow $ head hailstones) [] $ tail hailstones
    where
        hailstones = map parse $ take 5 xs

addRow :: (Position, Velocity) -> (Position, Velocity) -> [([Int], Int)] -> [([Int], Int)]
addRow ((x1, y1, _), (u1, v1, _)) ((xn, yn, _), (un, vn, _)) rows
    = ([vn - v1, u1 - un, y1 - yn, xn - x1], xn * vn - yn * un - x1 * v1 + y1 * u1) : rows

solve :: (Double, Double) -> [String] -> Int
solve bounds xs =
    length [ (head ts, y) | ts <- init $ tails stones, y <- tail ts, intersects bounds (head ts, y) ]
    where
        stones = map parse xs

intersects :: (Double, Double) -> (Stone, Stone) -> Bool
intersects _ (((x1, y1, _), (vx1, vy1, _)), ((x2, y2, _), (vx2, vy2, _)))
    | x1 == x2 && y1 == y2 && vx1 * vy2 == vx2 * vy1 = True
intersects _ ((_, (vx1, vy1, _)), (_, (vx2, vy2, _)))
    | vx1 * vy2 == vx2 * vy1 = False
intersects (lbound, ubound) (((x1, y1, _), (vx1, vy1, _)), ((x2, y2, _), (vx2, vy2, _)))
    = t1 > 0 && t2 > 0 && x >= lbound && x <= ubound && y >= lbound && y <= ubound
        where
            x = fromIntegral x1 + t1 * fromIntegral vx1
            y = fromIntegral y1 + t1 * fromIntegral vy1
            t2 = fromIntegral (vx1 * (y2 - y1) - vy1 * (x2 - x1)) /
                fromIntegral (vy1 * vx2 - vx1 * vy2)
            t1 = (fromIntegral (x2 - x1) + fromIntegral vx2 * t2) / fromIntegral vx1

parse :: String -> (Position, Velocity)
parse xs = case words xs of
    [x, y, z, _, vx, vy, vz] ->
        ((read $ init x, read $ init y, read z), (read $ init vx, read $ init vy, read vz))
    _ -> error $ "Cannot parse: " ++ xs