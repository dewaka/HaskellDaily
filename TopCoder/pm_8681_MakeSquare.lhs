http://community.topcoder.com/stat?c=problem_statement&pm=8681

A string is called a square if it is the concatenation of two copies of the same
string. For example, strings abcabc and aaaa are squares, strings aaa, abcab,
and defgh are not.

Given a string S, a step is one of the following changes:

Changing a single letter in S to any other letter.
Erasing a single letter from S.
Adding one new letter anywhere into S, including the beginning or the end of S.

For example, if S=abaca, then each of the strings abeca, baca, abafca, and
gabaca can be reached from S in a single step. You need at least two steps
to reach the string bac, at least four steps to reach the string dafg, and at
least five steps to reach the empty string.

You are given a String S. Return the smallest number of steps necessary to
change S into a square.

> isSqure :: String -> Bool
> isSqure s = even len && hs == ts
>   where
>     len = length s
>     hs = take (len `div` 2) s
>     ts = drop (len `div` 2) s

There are different strategies possible to make a given string S into a squred
one (if not already).
To find the minimum number of steps to make that happen I think we need to try
out multiple methods.

We need a measurement as to know how much off a given string from a squared one.
How to calculate this one.
For example we can say: "aba" is just one off from a squared string as we can
just delete the 'b' to get "aa" which is a squared string.

"abc" is worse than "aba" for example. So how can we quantify that?
I can say it's index is 2.

> offIndex :: String -> Int
> offIndex s = length ts - maxSubLength hs ts
>   where
>     hs = take (length s `div` 2) s
>     ts = drop (length s `div` 2) s

> subLength ns hs = go ns hs 0
>   where
>     go (x:xs) (y:ys) n
>       | x == y = go xs ys (n+1)
>       | otherwise = n
>     go _ _ n = n

> maxSubLength sub hay = maximum [ subLength sub hs | n <- [0..length hay - length sub]
>                                                   , let hs = drop n hay ]

> offIndex2 s = if odd $ length s
>               then idx + 1
>               else idx
>   where
>     idx = sum $ zipWith (\x y -> if x==y then 0 else 1) hs ts
>     hs = take (length s `div` 2) s
>     ts = drop (length s `div` 2) s

> main :: IO ()
> main = do
>   putStrLn "*** Solution for MakeSquare ***"
>   answer

> answer = undefined
