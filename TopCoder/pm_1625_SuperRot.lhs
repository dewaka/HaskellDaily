http://community.topcoder.com/stat?c=problem_statement&pm=1625

Problem Statement: SuperRot

One of the problems with most implementations is that everything is converted to
upper case. Another problem is that numbers are ignored completely, leaving them
unencrypted.

One way to overcome these limitations is to extend ROT13 to cover lowercase
letters as well as numbers. Here is how our extended ROT transformations will work:

characters   become
   A-M        N-Z
   N-Z        A-M
   a-m        n-z
   n-z        a-m
   0-4        5-9
   5-9        0-4

For example, the message "Uryyb 28" would become "Hello 73" after being transformed.
   U -> H        2 -> 7
   r -> e        8 -> 3
   y -> l
   y -> l
   b -> 0
Notice that the spaces were left as is.

You have intercepted a message which you believe to be encrypted using this process.

> import Data.Char (ord, chr)

> rot13 c
>   | c `elem` ['a'..'z'] = chr $ ord 'a' + (ord c - ord 'a' + 13) `rem` 26
>   | c `elem` ['A'..'Z'] = chr $ ord 'A' + (ord c - ord 'A' + 13) `rem` 26
>   | c `elem` ['0'..'9'] = chr $ ord '0' + (ord c - ord '0' + 5) `rem` 10
>   | otherwise = c

> codeRot13 = map rot13

> main :: IO ()
> main = do
>   putStrLn "*** Solution to SuperRot ***"
>   answer

> answer = mapM_ (print . codeRot13) examples
>   where
>     examples = [ ""
>                , "5678901234"
>                , "NnOoPpQqRr AaBbCcDdEe"
>                , "Gvzr vf 54 71 CZ ba Whyl 4gu bs gur lrne 7558 NQ"
>                , "Gur dhvpx oebja sbk whzcf bire n ynml qbt"
>                ]
