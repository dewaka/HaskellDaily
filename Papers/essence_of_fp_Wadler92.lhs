I'm getting started on Literate Haskell programming with a paper by none other than Dr. Philp Wadler.
The aim of this sheet is to go through, study and implement the paper's code and gain a better understanding.

http://www.eliza.ch/doc/wadler92essence_of_FP.pdf

Required imports

> import Control.Monad

As a warm up for Literate Haskell programming here's the canonical factorial function.

> fact 0 = 1
> fact 1 = 1
> fact n = n * fact (n-1)

********************************************************************************

Start of the paper.

Wadler discusses the kind of higher level refactoring enabled by Mondad by implementing a
simple interpreter for a toy language.

Basic data types of the language

> type Name = String
>
> data Term = Var Name
>           | Con Int
>           | Add Term Term
>           | Lam Name Term
>           | App Term Term
>
> data Value m = Wrong
>              | Num Int
>              | Fun (Value m -> m (Value m))
>
> type Environment m = [(Name, Value m)]
>
> showval :: Value m -> String
> showval Wrong = "<wrong>"
> showval (Num n) = show n
> showval (Fun _) = "<function>"
>
> interp :: Term -> Environment m -> Value m
> interp (Var x) e = lookupEnv x e
>
> lookupEnv :: Name -> Environment m -> Value m
> lookupEnv x [] = return Wrong
> lookupEnv x _ = undefined
