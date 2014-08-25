http://community.topcoder.com/stat?c=problem_statement&pm=10547

Let f(x) be the greatest odd divisor of x, where x is a positive integer.
You are given a positive integer N. Return f(1)+f(2)+?+f(N).

Example:
7
Returns: 21
f(1)+f(2)+f(3)+f(4)+f(5)+f(6)+f(7)=1+1+3+1+5+3+7=21

> divisors num = [n | n <- [1..num], num `rem` n == 0]

> grtOddDivisor 0 = error "0 has no odd divisors"
> -- Every number has 1 as a divisor, thus head will work here
> -- We have to reverse here becuase the larger divisors will be at the end
> grtOddDivisor num = head $ reverse $ filter odd $ divisors num

> sumOddDivisors num= sum $ map grtOddDivisor [1..num]

> answer = mapM_ (print . sumOddDivisors) examples
>   where
>     examples = [ 7
>                , 1
>                , 777
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to VolumeDiscount ***"
>   answer
