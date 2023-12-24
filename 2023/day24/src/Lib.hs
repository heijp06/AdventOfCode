module Lib
    ( parse
    , part1
    , part2
    , solve
    ) where

import Data.List (tails)

import Debug.Trace (trace)
import Text.Printf (printf)

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
intersects _ (((x1, _, _), (vx1, _, _)), ((x2, _, _), (vx2, _, _)))
    | x1 == x2 && vx1 /= vx2 = False
intersects _ (((_, y1, _), (_, vy1, _)), ((_, y2, _), (_, vy2, _)))
    | y1 == y2 && vy1 /= vy2 = False
intersects _ (((x1, _ , _), (vx1, _, _)), ((x2, _ , _), (vx2, _, _)))
    | x1 /= x2 && vx1 == vx2 = False
intersects _ (((_, y1, _), (_, vy1, _)), ((_, y2, _), (_, vy2, _)))
    | y1 /= y2 && vy1 == vy2 = False
intersects (lbound, ubound) (((x1, y1, _), (vx1, vy1, _)), ((x2, y2, _), (vx2, vy2, _)))
    | x1 == x2 && vx1 == vx2 = t > 0 && y >= lbound && y <= ubound
        where
            t = fromIntegral (y2 - y1) / fromIntegral (vy1 - vy2)
            y = fromIntegral y1 + t * fromIntegral vy1
intersects (lbound, ubound) (((x1, y1, _), (vx1, vy1, _)), ((x2, y2, _), (vx2, vy2, _)))
    | y1 == y2 && vy1 == vy2 = t > 0 && x >= lbound && x <= ubound
        where
            t = fromIntegral (x2 - x1) / fromIntegral (vx1 - vx2)
            x = fromIntegral x1 + t * fromIntegral vx1
intersects (lbound, ubound) (((x1, y1, _), (vx1, vy1, _)), ((x2, y2, _), (vx2, vy2, _)))
    = tx > 0 && x >= lbound && x <= ubound && ty > 0 && y >= lbound && y <= ubound
        where
            tx = fromIntegral (x2 - x1) / fromIntegral (vx1 - vx2)
            x = fromIntegral x1 + tx * fromIntegral vx1
            ty = fromIntegral (y2 - y1) / fromIntegral (vy1 - vy2)
            y = fromIntegral y1 + ty * fromIntegral vy1

parse :: String -> (Position, Velocity)
parse xs = case words xs of
    [x, y, z, _, vx, vy, vz] ->
        ((read $ init x, read $ init y, read z), (read $ init vx, read $ init vy, read vz))
    _ -> error $ "Cannot parse: " ++ xs