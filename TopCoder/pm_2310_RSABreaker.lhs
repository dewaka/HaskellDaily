http://community.topcoder.com/stat?c=problem_statement&pm=2310

> primeFactors num = [ p | p <- takeWhile (<= num `div` 2) primes
>                        , num `rem` p == 0 ]

> primes, nonPrimes :: [Integer]
> primes = [2,3,5] ++ (diff [7,9..] nonPrimes)
> nonPrimes = foldr1 f $ map g $ tail primes
>   where
>     f (x:xs) ys = x : merge xs ys
>     g p = [n * p | n <- [p,p+2..]]

> merge :: Ord a => [a] -> [a] -> [a]
> merge xs@(x:xs') ys@(y:ys') =
>   case compare x y of
>    LT -> x : merge xs' ys
>    EQ -> x : merge xs' ys'
>    GT -> y : merge xs  ys'

> diff :: Ord a => [a] -> [a] -> [a]
> diff xs@(x:xs') ys@(y:ys') =
>   case compare x y of
>    LT -> x : diff xs' ys
>    EQ -> diff xs' ys'
>    GT -> diff xs ys'

> computeM num =
>   let pfactors = primeFactors num
>   in length [ n | n <- [1..num-1]
>                 , not $ any (\t -> n `rem` t == 0) pfactors ]

> computeD e m =
>   -- This is the solution to modulo equation: x*e `mod` m = 1
>   -- x = m * t + (m - e) where t <- [0..]
>   [m * t + (m-e) | t <- [0..]]

> breakRSA e n x =
>   let m = computeM n
>   in m

> main :: IO ()
> main = do
>   putStrLn "*** Solution for RSABreaker ***"
>   answer

> answer = undefined
