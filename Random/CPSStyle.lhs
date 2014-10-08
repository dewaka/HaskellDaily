Continuation Passing Style

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
