module Helpers where

import Data.List


-- wraps a structure factor for an algebra that has Identity as first element
-- (i.e. Identity + a Lie algebra)
wrapIdentity :: (Num k) => Int -> Int -> Int -> (Int -> Int -> Int -> k) -> k
wrapIdentity 0 a b _ = if a == b then 1 else 0
wrapIdentity a 0 b _ = if a == b then 1 else 0
wrapIdentity a b 0 _ = if a == b then 1 else 0
wrapIdentity a b c f = f a b c

-- Uses a matrix representation to determine structure factors. This may be
-- useful if the matrix representation is known
fromMatrixRepr :: (Fractional k) => [[[k]]] -> Int -> Int -> Int -> k
fromMatrixRepr repr x y z = matrixProj (matrixMult (repr !! x) (repr !! y)) (repr !! z)

matrixMult :: (Num k) => [[k]] -> [[k]] -> [[k]]
matrixMult m1 m2 =
    map (\x -> map (\y -> sum $ map (\z -> (m1 !! x !! z) * (m2 !! z !! y))
    [0..(length (head m1) - 1)]) [0..(length (head m2) - 1)]) [0..(length m1 - 1)]

matrixTr :: (Num k) => [[k]] -> k
matrixTr m = sum $ map (\x -> m !! x !! x) [0..(length m - 1)]

matrixProj :: (Num k, Fractional k) => [[k]] -> [[k]] -> k
matrixProj m1 m2 = matrixTr (matrixMult m1 m2) / matrixTr (matrixMult m2 m2)

-- An equivalence relation of two lists as sets, useful to dispatch structure
-- to subsets of the algebra. Example:
-- structure a b c | [a,b,c] =~ [1,2,3] = someStructureOn123 a b c
(=~) :: (Eq a) => [a] -> [a] -> Bool
(=~) x y = null (x \\ y)
