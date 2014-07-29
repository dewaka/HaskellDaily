http://community.topcoder.com/stat?c=problem_statement&pm=9759

Problem Statement: UnknownNames

You are given a String[] questionMarkNames, where each element represents a
single name, and all names have the same length. Some of the letters are
missing, and those letters are represented by question marks ('?'). Arrange the
names in a vertical row, such that corresponding characters in each name are in
the same column. Then, order the names so that each column is sorted in
non-decreasing order from top to bottom. You can replace each question mark with
any letter to achieve this.

Return a String[] containing the lexicographically earliest ordering that you
can achieve. If there is no way to achieve the goal, return an empty String[]
instead. An ordering A comes before an ordering B if A contains an
alphabetically earlier name at the first index where they differ.

********************************************************************************

A worked out example

{"?ED?", "TO??", "????"}
Returns: {"AAAA", "AEDA", "TODA" }
If we make the order of names
0 1 2
then the lexicographically earliest ordering is
AEDA
TODA
TODA

0 2 1
AEDA
AEDA
TOEA

1 0 2
TO??
TED?
T???
- IMPOSSIBLE for this order, because O is after E, etc.

> import Data.List (transpose)

> validOrdering :: [String] -> Bool
> validOrdering xs = all checkValid $ transpose xs
>   where
>     checkValid (x:y:xs) = x <= y && checkValid xs
>     checkValid _ = True

> main :: IO ()
> main = do
>   putStrLn "*** UnknownNames Solution ***"
