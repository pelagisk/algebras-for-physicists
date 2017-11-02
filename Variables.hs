{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Variables where


import Data.Complex
import Data.Fixed
import Math.Algebras.Structures
import Math.CommutativeAlgebra.Polynomial


-- Type class for things which use complex numbers
class (Eq k, Num k) => IncludesComplex k where
    i :: k

-- Newtype wrapping Complex Double: this is a compromise to have better printing
-- of complex numbers
newtype C = C (Complex Double) deriving (Eq, Num, Floating, Fractional)

-- Just defining better printing of complex numbers
instance Show C where
     show (C (x :+ y)) | (x == 0) && (y == 1) = "i"
                       | (x == 0) && (y == -1) = "-i"
                       | x == 0 = show' y ++ "i"
                       | y == 0 = show' x
                       | y > 0 = show' x ++ "+" ++ show' y ++ "i"
                       | otherwise = show' x ++ "-" ++ show' (negate y) ++ "i"
                       where show' z | zrem == 0 = show (floor z :: Integer)
                                     | otherwise = show z
                                     where zrem = z `mod'` 1.0 :: Double

-- C is of course an instance, since it has complex numbers
instance IncludesComplex C where
    i = C (0 :+ 1)

-- Var is a type synonym (another name) for an algebra of commuting variables
-- and complex numbers. Here it plays the double role as the "c-number" field
-- for non-commuting algebras defined over it
type Var = GlexPoly C String

-- Var is defined over complex numbers as well, so it is an instance like C
instance IncludesComplex Var where
     i = unit i  -- using the property of Var being an algebra

-- New variables can be made like so: x = var "x"
var :: String -> Var
var s = Math.CommutativeAlgebra.Polynomial.var s :: Var
