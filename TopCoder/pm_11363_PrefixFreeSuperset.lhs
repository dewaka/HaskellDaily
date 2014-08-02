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

Testing isPrefixFreeSet function

> test1 = isPrefixFreeSet ["00", "01", "110", "10"] == True
> test2 = isPrefixFreeSet ["0","01"] == False
> test3 = isPrefixFreeSet ["111","11","1"] == False

> testInputs = [ (["010"], 4)
>              , (["01","000"], 4)
>              , (["0011","011110101","11101010111","11101010100000000","11101010100000001111"], 1000000000000)
>              , (["010","00","011","1"], 4) ]


> main :: IO ()
> main = do
>   putStrLn "*** PrefixFreeSuperset Solution ***"
