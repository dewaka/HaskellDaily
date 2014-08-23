http://community.topcoder.com/stat?c=problem_statement&pm=3490

Problem Statement: CmpdWords

Some languages allow the use of compound words, words that are the concatenation
of words from the language's dictionary.
We have defined a language that consists of a dictionary of distinct words.
We want to know whether we should allow the use of compound words constructed by
concatenating exactly two distinct dictionary words. The potential problem is
that a compound word is ambiguous if either (or both) of the following
conditions applies:

* the compound word is a dictionary word
* the compound word can be formed in more than one way.
For example, if "am","eat","a", "meat", "hook", and "meathook" were all in the
dictionary, then "meathook" would be ambiguous according to the first condition,
and "ameat" would be ambiguous according to the second condition.

Create a class CmpdWords that contains a method ambiguous that is given a
String[] dictionary and that returns the number of ambiguous words that would
result from allowing the compounding of distinct pairs of dictionary words.
An ambiguous word should be counted only once, no matter how many different
ways it could be formed.

Note that compound words are NOT added to the dictionary.
So the dictionary {"a", "b","c"} does not allow "abc" as a compound word.

> import Data.List (tails, nub)

> isAmbiguous dict  word = inDictionary || multiCompoundWays
>   where
>     inDictionary = word `elem` dict
>     multiCompoundWays = count word (compounds dict) > 1
>       where
>         count x = length . filter (==x)

We need a function to generate all possible compound words. Compound words
are formed by selecting 2 words from a given dictionary and concatenating them.

> combinations 0 _ = [[]]
> combinations n xs = [ y:ys' | y:ys <- tails xs
>                             , ys' <- combinations (n-1) ys]

> compounds = concat . map (\[a, b] -> [a++b, b++a]) . combinations 2

> examples = [ ["am","eat","a", "meat", "hook","meathook"]
>            , ["a","b","c"]
>            , ["a","aa","aaa"]
>            , ["abc","bca","bab","a"]
>            ]

Following function returns all distinct ambiguous words which can be formed
using the given dictionary.

> ambiguous dict =
>   nub $ filter (isAmbiguous dict) $ compounds dict

> answer = mapM_ (print . answer') examples
>   where
>     answer' ds =
>       let as = ambiguous ds
>       in (length as, as)

> main :: IO ()
> main = do
>   putStrLn "*** Solution for CmpdWords ***"
>   answer
