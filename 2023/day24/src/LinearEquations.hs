module LinearEquations
    ( solve
    ) where

-- This code does a halfbaked attempt at solving Ax = v by using Gaussian elimination.
-- The attempt is half baked because it is assumed that values on the diagonal of the
-- matrix and all intermediate matrices are not zero when they are used as pivot in the
-- function combineSolve. This is not true in general and could be worked around by
-- interchanging rows (not done here).
-- In solve matrix is A, vector is v and the return value is x.
solve :: [[Rational]] -> [Rational] -> [Rational]
solve matrix vector = map last $ foldl combineSolve augmentedMatrix [0..length matrix - 1]
    where
        augmentedMatrix = zipWith (++) matrix (map (:[]) vector)

combineSolve :: [[Rational]] -> Int -> [[Rational]]
combineSolve xss index = zipWith (applyPivot index row') xss [0..]
    where
        row = xss !! index
        pivot = row !! index
        row' = map (/pivot) row

applyPivot :: Int -> [Rational] -> [Rational] -> Int -> [Rational]
applyPivot pivotIndex pivotRow _ rowIndex | pivotIndex == rowIndex = pivotRow
applyPivot pivotIndex pivotRow row _ = zipWith (-) row $ map (factor*) pivotRow
    where
        factor = row !! pivotIndex
