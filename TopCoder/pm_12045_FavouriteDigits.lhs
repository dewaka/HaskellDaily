http://community.topcoder.com/stat?c=problem_statement&pm=12045

Problem Statement: FavouriteDigits

Tetka Rapotacka just heard a juicy rumor about one of her neighbors: poor Jachym
has lost N kacky (their local monetary unit) on a slot machine yesterday. Tetka
Rapotacka now plans to share this rumor with everyone, including Tetka
Klepetacka. Tetka Rapotacka has two favorite digits: digit1 and digit2. She
only likes numbers that contain at least count1 occurrences of digit1 and at
least count2 occurrences of digit2. When gossiping, she only uses numbers she
likes. So if she does not like the number N, she will increase it until she
finds a number that she likes.

You are given the long N and the four ints digit1, count1, digit2, and count2.
Return the smallest integer that is greater than or equal to N, contains at
least count1 occurrences of digit digit1, and at least count2 occurrences of
digit digit2.

Example 1:
47
1
0
2
0

Returns: 47
As count1=0 and count2=0, Tetka Rapotacka likes all numbers.

> import Data.List (find)

> countDigits d num =
>   length $ filter (==c) (show num)
>   where
>     (c:_) = show d

> answer num d1 c1 d2 c2 =
>   case find (\n -> (countDigits d1 n >= c1) && (countDigits d2 n >= c2)) [num..] of
>     Nothing -> error "Couldn't find such a number!"
>     Just num -> num

> main :: IO ()
> main = do
>   putStrLn "*** FavouriteDigits Solution ***"
>   print $ answer 47 1 0 2 0
>   print $ answer 47 5 0 9 1
>   print $ answer 47 5 0 3 1
>   print $ answer 47 2 1 0 2
>   print $ answer 123456789012345 1 2 2 4
>   print $ answer 92 1 1 0 0
