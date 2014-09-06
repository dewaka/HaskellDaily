> calculateLength [] _ _ = 0
> calculateLength [x] s p =
>   let n:_ = dropWhile (<s) [2^i | i <- [p..]]
>   in x+n-1
> calculateLength (x:xs) s p =
>   let (n,j):_ = dropWhile ((<s) . fst) [(2^i, i) | i <- [p..]]
>   in calculateLength xs (x+n) j

> main :: IO ()
> main = do
>   putStrLn "*** Solution for SignalIntelligence ***"
