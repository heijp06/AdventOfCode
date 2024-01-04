module Lib
    ( matrix
    , parse
    , part1
    , part2
    , solve
    ) where

import Data.List (tails)
import qualified LinearEquations as LE

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Stone = (Position, Velocity)

part1 :: [String] -> Int
part1 = solve (200000000000000, 400000000000000)

part2 :: [String] -> Int
part2 xs = round (fromRational $ head xyuv + (xyuv !! 1) + (wz !! 1) :: Double)
    where
        m = matrix xs
        xyuv = LE.solve (map (map toRational . fst) m) (map (toRational . snd) m)
        wz = solveWz xs (head xyuv) (xyuv !! 2)

solveWz :: [String] -> Rational -> Rational -> [Rational]
solveWz xs x u = LE.solve [row1, row2] [v1, v2]
    where
        (x1, z1, u1, w1, x2, z2, u2, w2)
            = case take 2 $ map parse xs of
                [((x1', _, z1'), (u1', _, w1')), ((x2', _, z2'), (u2', _, w2'))] -> (x1', z1', u1', w1', x2', z2', u2', w2')
                _ -> error "Cannot parse"
        row1 = [toRational x1 - x, u - toRational u1]
        row2 = [toRational x2 - x, u - toRational u2]
        v1 = (toRational x1 - x) * toRational w1 - (toRational u1 - u) * toRational z1
        v2 = (toRational x2 - x) * toRational w2 - (toRational u2 - u) * toRational z2

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