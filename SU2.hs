{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |A module defining the algebra of SU2 over a field of complex variables
module SU2 where


import Prelude hiding ( (*>) )
import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Helpers
import Variables
import LeviCivita


newtype PauliBasis = P Int deriving (Eq, Ord)

type Pauli = Vect Var PauliBasis

instance Show PauliBasis where
    show (P 0) = "I"
    show (P 1) = "σx"
    show (P 2) = "σy"
    show (P 3) = "σz"
    show (P x) = error ("Input P " ++ show x ++ " not in basis")

instance Algebra Var PauliBasis where
    unit x = x *> return (P 0)
    mult = linear mult'
         where mult' (P x, P y) = sum $ map (term x y) [0..3]
               term x y z = i *> unit (wrapIdentity x y z leviCivita) *> return (P z)

id',sx,sy,sz :: Pauli
id' = return (P 0)
sx  = return (P 1)
sy  = return (P 2)
sz  = return (P 3)
