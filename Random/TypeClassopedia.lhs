> import Control.Monad

MonadPlus

Pythagorian triplets are usually written as follows.

> pythag = [ (x, y, z) | z <- [1..]
>                      , x <- [1..z]
>                      , y <- [x..z]
>                      , x^2 + y^2 == z^2 ]

Using MonadPlus properties of Lists we can write the same generator as follows.

> pythag' = do
>   z <- [1..]
>   x <- [1..z]
>   y <- [x..z]
>   guard $ x^2 + y^2 == z^2
>   return (x, y, z)
