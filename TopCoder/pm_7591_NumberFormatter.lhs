http://community.topcoder.com/stat?c=problem_statement&pm=7591

Problem Statement for NumberFormatter

You are a developer for a financial computer system. Users of the system often
operate with large numbers, so it's important that numbers be formatted in an
easy-to-read manner. You are given a String number containing the internal
representation of a non-negative number. This number will contain only digits,
along with an optional decimal separator represented by a comma (',').
Format the number as follows:

1. If the number contains a decimal separator, replace the decimal separator
with a period ('.').

2. Divide the integer part of the number into groups of three consecutive digits,
from right to left. Depending on the number of digits, the left-most group might
only contain 1 or 2 digits. Insert a single space (' ') between each pair of
consecutive groups. (The integer part of the number is the part of the number to
the left of the decimal separator if one exists, or the entire number if there is
no decimal separator.)

For example, "1234567,890" would be formatted as "1 234 567.890" and "1024" would be
formatted as "1 024". If insignificant leading or trailing zeroes exist in the number,
they must be preserved.
For example, "00003,1234000" would be formatted "00 003.1234000".
Return the formatted version of the given number.

> import Data.List (intersperse)

> groupBy :: Int -> [a] -> [[a]]
> groupBy _ [] = []
> groupBy n xs = take n xs : groupBy n (drop n xs)

> formatNum :: String -> String
> formatNum xs =
>   let fullPart = takeWhile (/=',') xs
>       decPart = if ',' `elem` xs
>                 then tail $ dropWhile (/=',') xs 
>                 else []
>       n = length fullPart
>       s = take (n `rem` 3) fullPart
>       rest = drop (n `rem` 3) fullPart
>       fmtFirst = concat $ intersperse " " (s : groupBy 3 rest)
>   in if decPart == [] then fmtFirst
>      else fmtFirst ++ "." ++ decPart
 
> main :: IO ()
> main = do
>   putStrLn "*** NumberFormatter Solution ***"
>   putStrLn $ formatNum "00003,1234000"
>   putStrLn $ formatNum "1024"
>   putStrLn $ formatNum "1234567,890"
