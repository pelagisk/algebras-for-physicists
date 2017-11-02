{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |A module defining the algebra of the harmonic oscillator over a field of complex variables
module Harmonic where


import Prelude hiding ( (*>) )
import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Variables

import Data.Char.SScript

infixl 5 :*:
infixr 6 :^:

data HarmonicBasis = I
                   | B
                   | BH
                   | HarmonicBasis :*: HarmonicBasis
                   | HarmonicBasis :^: Int
                   deriving (Eq, Ord)

instance Show HarmonicBasis where
                   show I = "I"
                   show B = "a"
                   show BH = "c"
                   show (u :*: v) = show u ++ "*" ++ show v
                   -- the code below seems to lead to double superscript.
                   -- only map first?
                   show (u :^: n) = show u ++ map superscript (show n)

type Harmonic = Vect Var HarmonicBasis

instance Algebra Var HarmonicBasis where
    unit x = x *> return I
    mult = linear mult'
         where mult' (I, u) = return u
               mult' (u, I) = return u
               mult' (B, BH) = (-1) *> id' + mult' (BH, B)
               mult' (B, B) = return (B :^: 2)
               mult' (BH, BH) = return (BH :^: 2)
               mult' (u, v :*: w) | u == v = mult' (u :^: 2, w)
                                  | v == w = mult' (u, v :^: 2)
                                  | otherwise = return u * mult' (v, w)
               mult' (u :^: n, v) | u == v = return $ u :^: (n+1)
               mult' (u :^: n, v :^: m) | u == v = return $ u :^: (n+m)
               mult' (u, v :^: n) | u == v = return $ u :^: (n+1)
               mult' (u :*: v, w) | v == w = return (u :*: (v :^: 2))
                                  | u == v = return (u :^: 2 :*: w)
                                  | otherwise = return u * mult' (v, w)
               mult' (u,v) = return (u :*: v)

id',a,c :: Harmonic
id' = return I
a   = return B
c   = return BH
