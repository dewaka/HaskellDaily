http://www.reddit.com/r/dailyprogrammer/comments/12csm4/10302012_challenge_109_intermediate/

Palindrome integer problem.
First we'll write a function to check whether an Integer is a palindrome.
One very easy way to do that is to check it as a string whether it's a palindrome.
First work on that solution which should be correct and then try to find an arithmatic based
potentially faster one.

> palindrome :: String -> Bool
> palindrome s = s == reverse s

> palindromeNum :: (Num t, Show t) => [t] -> [t] -> [(t, t)]
> palindromeNum xs ys = map snd $ filter palindromePair [(x*y, (x,y)) | x <- xs, y <- ys]
>   where
>     palindromePair (p, _) = palindrome $ show p

> main :: IO ()
> main = do
>   putStrLn "Solution to first example problem: xs <- [90..99] and yx <- [90..99]"
>   print $ palindromeNum [90..99] [90..99]
>   putStrLn ""
>   putStrLn "Solution to second example problem: xs <- [10..11] and yx <- [10..11]"
>   print $ palindromeNum [10..11] [10..11]
