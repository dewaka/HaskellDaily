> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

Data types a la carte paper by Wouther Swierstra

> data Expr' = Val' Int | Add' Expr' Expr'

> eval' :: Expr' -> Int
> eval' (Val' x) = x
> eval' (Add' x y) = eval' x + eval' y

> render :: Expr' -> String
> render (Val' x) = show x
> render (Add' x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

The problem with above definitions are they are not extensible with respect to
- adding new cases
- adding new functions
without having to compile everything again.

In this paper Swierstra proposes a solution to this "Expression problem" as
noted by Wadler (1998)

Haskell can add functions without any problem, but adding constructors requires
recompiling old code.

Fixing the expression problem

> data Expr f = In (f (Expr f))

> data Val e = Val Int
> type IntExpr = Expr Val

> data Add e = Add e e
> type AddExpr = Expr Add

> data (f :+: g) e = Inl (f e) | Inr (g e)

> addExample :: Expr (Val :+: Add)
> addExample = In addExpr
>   where addExpr = Inr (Add val1 val2)
>         val1 = In (Inl (Val 25))
>         val2 = In (Inl (Val 5))

Evaluation

> instance Functor Val where
>   fmap f (Val x) = Val x

> instance Functor Add where
>   fmap f (Add e1 e2) = Add (f e1) (f e2)

> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap f (Inl e1) = Inl (fmap f e1)
>   fmap f (Inr e2) = Inr (fmap f e2)

> foldExpr :: Functor f => (f a -> a) -> Expr f -> a
> foldExpr f (In t) = f (fmap (foldExpr f) t)

> class Functor f => Eval f where
>   evalAlgebra :: f Int -> Int

> instance Eval Val where
>   evalAlgebra (Val x) = x

> instance Eval Add where
>   evalAlgebra (Add e1 e2) = e1 + e2

> instance (Eval f, Eval g) => Eval (f :+: g) where
>   evalAlgebra (Inl x) = evalAlgebra x
>   evalAlgebra (Inr y) = evalAlgebra y

> eval :: Eval f => Expr f -> Int
> eval expr = foldExpr evalAlgebra expr

Now he defines smart constructors to build expressions

> class (Functor sub, Functor sup) => sub :<: sup where
>   inj :: sub a -> sup a

> instance Functor f => f :<: f where
>   inj = id

-- > instance (Functor f, Functor g) => f :<: (f :+: g) where
-- >   inj = Inl

-- > instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
-- >   inj = Inr . inj
