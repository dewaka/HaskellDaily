{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
http://community.topcoder.com/stat?c=problem_statement&pm=11363

Problem Statement: PrefixFreeSuperset

A prefix of a string s is any string that can be obtained by erasing zero or
more characters from the right end of s. A prefix-free set is a set of binary
words in which no element is a prefix of another element in the set.
For example {"00"} , {"00", "01", "110", "10"} and the empty set are examples
of prefix-free sets. On the other hand, {"0","01"} and {"111","11","1"} are
not prefix-free.

You will be given a String[] cur containing a prefix-free set of binary words,
and a long k. Return the minimum sum of the lengths of all the words in a set
of exactly k binary words that is prefix-free and includes all words from cur.
If such a set does not exist, return -1. If the number to be returned is
strictly greater than 1000000000000000000 (10^18), return -2 instead.

> import Data.List (isPrefixOf)

> isPrefixFreeSet xs = length xs == countPrefixes xs
>   where
>     countPrefixes = sum . map countP
>     countP x = length $ filter (isPrefixOf x) xs

The above function is correct but super inefficient since it will keep
processing other elements even if it finds a violating instance.

> isPrefixFreeSet' ys = go ys
>   where
>     countPrefix x = length $ filter (isPrefixOf x) ys
>
>     go [] = True
>     go (x:xs) = (countPrefix x == 1) && go xs
>

Testing isPrefixFreeSet function

> test1 = isPrefixFreeSet ["00", "01", "110", "10"] == True
> test2 = isPrefixFreeSet ["0","01"] == False
> test3 = isPrefixFreeSet ["111","11","1"] == False

> test1' = isPrefixFreeSet' ["00", "01", "110", "10"] == True
> test2' = isPrefixFreeSet' ["0","01"] == False
> test3' = isPrefixFreeSet' ["111","11","1"] == False

To solve this problem we have to generate prefix free sets which will be of
given length and also containing all the words of the given list.

Let's denote the given set as S and the given number as n. Element of S are
denoted by E1, E2,..., Em notation where m is the length of S.
That is,
  S = [E1, E2, ..., Em]

The solution (prefix free set) would be of the following form which is denoted
by P,
  P = [X1, X2, ..., E1, E2, ..., Em] where Xi is a binary word.
The general binary word Xj belonging to P should not be a prefix of any other
element in P. In other words when j != k for Xj `elemtOf` P & Xk `elementOf` P
Xj `isPrefixOf` Xk && Xk `isPrefixOf` Xj is False for all.

Thus by that definition we can quickly check whether it is possible to generate
such a prefix set (P) by the given S. If the given is not itself a prefix free
set this task is impossible since every element of S should be in P as well.

There's another restriction which limits the possibility of finding a solution.
That is the number n which restricts the number of elements P can have.

> minPossiblePFreeSetLength xs n =
>   if (not $ isPrefixFreeSet xs) || (length xs > n)
>   then -1
>   else
>     let minPSet = findMinPSet xs n
>     in sum $ map length minPSet

> findMinPSet xs n = undefined

> testInputs = [ (["010"], 4)
>              , (["01","000"], 4)
>              , (["0011","011110101","11101010111","11101010100000000","11101010100000001111"], 1000000000000)
>              , (["010","00","011","1"], 4) ]

> main :: IO ()
> main = do
>   putStrLn "*** PrefixFreeSuperset Solution ***"
