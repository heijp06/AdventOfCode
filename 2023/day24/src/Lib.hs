module Lib
    ( parse
    , part1
    , part2
    , solve
    ) where

import Data.List (tails)

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Stone = (Position, Velocity)

part1 :: [String] -> Int
part1 = solve (200000000000000, 400000000000000)

part2 :: [String] -> Int
part2 = undefined

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