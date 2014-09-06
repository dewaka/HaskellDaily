> encodedLength xs = go xs 0 0
>   where
>     go [] _ _ = 0
>     go [x] s p =
>       let n:_ = dropWhile (<=s) [2^i | i <- [p..]]
>       in x+n-1
>     go (x:xs) s p =
>       let (n,j):_ = dropWhile ((<=s) . fst) [(2^i, i) | i <- [p..]]
>       in go xs (x+n) j

> selections [] = []
> selections (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- selections xs ]

> permutations [] = [[]]
> permutations xs = [ y:zs | (y, ys) <- selections xs
>                          , zs <- permutations ys ]

> encodings msg = [ (encodedLength m, m) | m <- permutations msg ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution for SignalIntelligence ***"

> example3 = [ 1000000000,1000000000,1000000000,1000000000,1000000000,
>              1000000000,1000000000,1000000000,1000000000,1000000000,
>              1000000000,1000000000,1000000000,1000000000,1000000000,
>              1000000000,1000000000,1000000000,1000000000,1000000000 ]
