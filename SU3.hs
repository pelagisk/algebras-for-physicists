{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |A module defining the algebra of SU3 over a field of complex variables
module SU3 where


import Prelude hiding ( (*>) )
import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Variables
import LeviCivita
import Helpers


newtype GellMannBasis = G Int deriving (Eq, Ord)

type GellMann = Vect Var GellMannBasis

instance Show GellMannBasis where
    show (G 0) = "I"
    show (G 1) = "λ1"
    show (G 2) = "λ2"
    show (G 3) = "λ3"
    show (G 4) = "λ4"
    show (G 5) = "λ5"
    show (G 6) = "λ6"
    show (G 7) = "λ7"
    show (G 8) = "λ8"
    show (G x) = error ("Input λ" ++ show x ++ " not in basis")

instance Algebra Var GellMannBasis where
    unit x = x *> return (G 0)
    mult = linear mult'
         where mult' (G x, G y) = sum $ map (term x y) [0..8]
               term x y z = unit (su3Structure x y z) *> return (G z)

id',l1,l2,l3,l4,l5,l6,l7,l8 :: GellMann
id' = return (G 0)
l1  = return (G 1)
l2  = return (G 2)
l3  = return (G 3)
l4  = return (G 4)
l5  = return (G 5)
l6  = return (G 6)
l7  = return (G 7)
l8  = return (G 8)

su3Structure :: (IncludesComplex k, Floating k) => Int -> Int -> Int -> k
su3Structure a b c = wrapIdentity a b c g
                   where g x y z = (1/2) * (su3D x y z + i * su3f x y z)

-- Structure constants of commutator (Lie) algebra
su3f :: Floating k => Int -> Int -> Int -> k
su3f a b c | [a,b,c] =~ [1,2,3] = leviCivita' [1,2,3] 2 a b c
           | [a,b,c] =~ [1,4,7] = leviCivita' [1,4,7] 1 a b c
           | [a,b,c] =~ [1,6,5] = leviCivita' [1,6,5] 1 a b c
           | [a,b,c] =~ [2,4,6] = leviCivita' [2,4,6] 1 a b c
           | [a,b,c] =~ [2,5,7] = leviCivita' [2,5,7] 1 a b c
           | [a,b,c] =~ [3,4,5] = leviCivita' [3,4,5] 1 a b c
           | [a,b,c] =~ [3,7,6] = leviCivita' [3,7,6] 1 a b c
           | [a,b,c] =~ [4,5,8] = leviCivita' [4,5,8] (sqrt 3) a b c
           | [a,b,c] =~ [6,7,8] = leviCivita' [6,7,8] (sqrt 3) a b c
           | otherwise = 0

-- Structure constants of anti-commutator algebra
-- {\lambda^a, \lambda^b} = \frac{2}{3} \delta^{ab} I + \sum_c d^{ab}_c \lambda^c
-- is here written as {\lambda^a, \lambda^b} = \sum_c D^{ab}_c \lambda^c
su3D :: Floating k => Int -> Int -> Int -> k
su3D 1 1 8 =  2/sqrt 3
su3D 2 2 8 =  2/sqrt 3
su3D 3 3 8 =  2/sqrt 3
su3D 8 8 8 = -2/sqrt 3
su3D 4 4 8 = -1/sqrt 3
su3D 5 5 8 = -1/sqrt 3
su3D 6 6 8 = -1/sqrt 3
su3D 7 7 8 = -1/sqrt 3
su3D 1 4 6 =  1
su3D 1 5 7 =  1
su3D 2 4 7 = -1
su3D 2 5 6 =  1
su3D 3 4 4 =  1
su3D 3 5 5 =  1
su3D 3 6 6 = -1
su3D 3 7 7 = -1
su3D a b 0 | a == b = 2/3
su3D _ _ _ = 0

-- One could also define structure using the matrix representation:
-- su3Structure :: (IncludesComplex k, Floating k) => Int -> Int -> Int -> k
-- su3Structure = fromMatrixRepr repr
--                where repr = [
--                             [[1, 0, 0], [0, 1, 0], [0, 0, 1]],
--                             [[0, 1, 0], [1, 0, 0], [0, 0, 0]],
--                             [[0, -i, 0], [i, 0, 0], [0, 0, 0]],
--                             [[1, 0, 0], [0, -1, 0], [0, 0, 0]],
--                             [[0, 0, 1], [0, 0, 0], [1, 0, 0]],
--                             [[0, 0, -i], [0, 0, 0], [i, 0, 0]],
--                             [[0, 0, 0], [0, 0, 1], [0, 1, 0]],
--                             [[0, 0, 0], [0, 0, -i], [0, i, 0]],
--                             [[s, 0, 0], [0, s, 0], [0, 0, -2*s]]
--                             ]
--                      s = 1/sqrt 3
