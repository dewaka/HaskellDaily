http://community.topcoder.com/stat?c=problem_statement&pm=8772

Problem Statement: StringInterspersal

A String S is an interspersal of a set of Strings W, if W is a set of disjoint
subsequences of S which cover S. Less formally, S can be formed from W by
partitioning each member of W into substrings, then concatenating all the
substrings, while maintaining the order of the substrings within each
element of W. For example, if W contains the
strings {"DESIGN", "ALGORITHM", "MARATHON"}, then one possible
interspersal would be "ADELGMAORARISIGNTHMTHON", formed as shown below.

 DE         SIGN

A  LG  O  RI    THM

     MA RA         THON

-----------------------

ADELGMAORARISIGNTHMTHON

Given a String[] W, return the lexicographically minimum interspersal of the
Strings in W

*** Solution *******************************************************************

> import Data.List (tails)

> intersperse :: [a] -> [a] -> [a]
> intersperse (x:xs) (y:ys) = [x, y] ++ intersperse xs ys
> intersperse _ _           = []


Now the question beccomes how to break things down
How to go about this

For a single intersperseStrings case we have to break down the 'xs' string to n
substrings. Then we have to breakdown 'ys' also to n substrings
Then we can go intersperse these substring lists...

We have to find all the ways to do this and that would be a stream

When it comex to breaking down ... is it going

> intersperseStrings xs ys = undefined

I think I might require combinations algorithm for this.

> combinations 0 _ = [[]]
> combinations n xs = [y:ys | y:xs' <- tails xs
>                           , ys <- combinations (n-1) xs']

> main :: IO ()
> main = do
>   putStrLn "*** StringInterspersal Solution ***"
