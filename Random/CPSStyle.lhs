Continuation Passing Style

> import Control.Monad.Cont

> add_cps :: Int -> Int -> ((Int -> r) -> r)
> add_cps x y k = k (x+y)

> square_cps :: Int -> ((Int -> r) -> r)
> square_cps x k = k (x*x)

> pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
> pythagoras_cps x y k =
>   square_cps x $ \x2 ->
>   square_cps y $ \y2 ->
>   add_cps x2 y2 k

Now a bit more complex example.

> thrice :: (a -> a) -> a -> a
> thrice f x = f $ f $ f x

The same function in CPS style would be,

> thrice_cps f x k =
>   f x $ \fx ->
>   f fx $ \ffx ->
>   f ffx k

Above function takes a cps form function as the first argument, namely f.
As a demostration of usage if we define,

> square x = x * x

Then the following two expressions are functionally equivalent

> res1 = print $ thrice square 2 -- prints 256
> res2 = thrice_cps square_cps 2 print -- prints 256

> fact n = product [1..n]

In a CPS version you never return from a function directly.
If you just want the result you can pass in fact_cps id which will be equivalent
to fact defined above.

> fact_cps n k = k $ product [1..n]

> greet pfx efx = do
>   name <- getLine
>   case name of
>    (c:_) | c `elem` ['a'..'c'] -> pfx name
>    otherwise -> efx name

http://blog.sigfpe.com/2008/12/mother-of-all-monads.html

Continuation Monad usage

> ex1 = do
>   a <- return 1
>   b <- return 10
>   return $ a+b

> test1 = runContT ex1 show     -- "11"

> ex2 = do
>   a <- return 1
>   b <- ContT (\_ -> "escape")
>   return $ a + b

> test2 = runContT ex2 show     -- "excape"

> ex3 = do
>   a <- return 1
>   b <- ContT (\f -> f 10 ++ f 20)
>   return $ a+b

> test3 = runContT ex3 show

Using the list Monad we get the follownig

> test4 = do                    -- test4 = [30,60,70,140]
>   a <- [3, 7]
>   b <- [10, 20]
>   return $ a*b

> ex4 = do
>   a <- return 3
>   b <- ContT (\f -> f 10 ++ f 20)
>   return $ a+b

Some examples from the paper 'Continuation-Based Program Transformations'

> reverse' ls =
>   case ls of
>    [] -> []
>    (x:xs) -> (reverse' xs) ++ [x]

> reverse'' = reverse_cps id

> reverse_cps cont ls =
>   case ls of
>    [] -> cont ls
>    (x:xs) -> reverse_cps cont' xs
>      where
>        cont' = \w -> cont (w ++ [x])

> map' f [] = []
> map' f (x:xs) = f x : map' f xs

> map_cps cont f [] = cont []
> map_cps cont f (x:xs) = map_cps cont' f xs
>   where
>     cont' = \t -> cont (f x : t)

> map'' = map_cps id
