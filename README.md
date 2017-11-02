# HaskellForMaths, use case for theoretical physicists

## Introduction

Often in theoretical physics, one needs to calculate commutators of complicated symbolic expressions in some non-commuting algebra. It is useful to be able to check calculations done by hand, and it should be a routine task anyway. This example code shows how one can use Haskell code to define and calculate in such algebras with the normal background of a working theorist.

Note that this is a use case! I have not written the library behind all the math, I'm just connecting the dots for physicists interested in using the library for practical calculations!

## Installation

First, install Haskell and its build system Cabal through the [Haskell Platform](https://www.haskell.org/downloads#platform).

Second, if you at any time feel the need to read up on Haskell (you may skip this step for now), [I recommend this introduction](http://learnyouahaskell.com/chapters) and [this blog](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/). You will not be disappointed.

Third, install the excellent package [HaskellForMaths](https://hackage.haskell.org/package/HaskellForMaths):

```bash
cabal update
cabal install haskellformaths
```

To learn more about Haskell and mathematics, check out the accompanying [blog](http://haskellformaths.blogspot.se). There are a lot of other interesting things to be learnt from Haskell for physicists and mathematicians.

## Use case

To use the algebra of Pauli matrices and the identity matrix, for example to calculate the equations of motions of some Hamiltonian, is simple. From the terminal:

```bash
ghci
```

and then write

```haskell
:load SU2.hs
let [j, h] = map var ["j", "h"]
let hamiltonian = j *> sx + h *> sz
let comm [x, y] = x * y - y * x
let eom o = i *> comm [hamiltonian, o]
eom sy
```

To define your own algebra, follow the pattern in these files.

## Credits

I want to thank David Amos and the Haskell community for helpful discussions and help with these basic things.

## License

BSD3 License, see the file `LICENSE`

## Author

The library is written by David Amos and licensed under a BSD3 License

