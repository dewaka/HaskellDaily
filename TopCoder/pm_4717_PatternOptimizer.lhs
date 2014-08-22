http://community.topcoder.com/stat?c=problem_statement&pm=4717

Problem Statement: PatternOptimizer

Some dictionaries use a word pattern that consists of letters, '?' symbols which
each denote exactly one letter, and '*' symbols which each denote zero or more
letters.

Interestingly, some patterns represent the same set of words.
For example, "*??*a" and "?*?a" (quotes for clarity only) patterns both
represent all words that consist of three or more letters and end with 'a'.

You will be given a String pattern. Your method should return the shortest
pattern that represents the same set of words as the given pattern.
Return the lexicographically first in case of tie.

To implement reductions we got to have functions to do string replacemnts.
We can use the simple startsWith function to check whether a pattern occurs at
the start of a given string (Eq-able list).

> startsWith :: Eq a => [a] -> [a] -> Bool
> startsWith (x:xs) (y:ys) = x == y && startsWith xs ys
> startsWith [] _ = True
> startsWith _ _ = False

This function will replace all instances of pattern xs in ls. This is a generic
function which will work on any type of list which supports Eq.

> replace :: Eq a => [a] -> [a] -> [a] -> [a]
> replace xs ys ls@(l:ls') =
>   if startsWith xs ls then ys ++ rs
>   else l:replace xs ys ls'
>   where
>     rs = replace xs ys $ drop (length xs) ls
> replace _ _ [] = []

0)
"*??*a"
Returns: "*??a"
1)
"*b**a*"
Returns: "*b*a*"
2)
"cla??"
Returns: "cla??"
3)
"*?*?*?*"
Returns: "*???"
4)
"T***nd?*"
Returns: "T*nd*?"

For this problem we should define a set of reductions which make pattern sense.
Some reductions:
   ** -> *
   ?* -> *? (Although it does not reduce,
                 it is simpler in terms of operator precedence)

We can implement the multi-star reduction as follows.
Any contiguous occurance of multiple stars can be reduced to a single star.

> reduceMultiStar ps =
>   let rs = replace "**" "*" ps
>   in if rs == ps
>      then rs -- if no change from original, then reductions are complete
>      else reduceMultiStar rs

Focusing on mixed * and ? patterns. It is very clear ?* to be simplified to *?.

> reduceQuestionStar ps =
>   let rs = replace "?*" "*?" ps
>   in if rs == ps
>      then rs
>      else reduceQuestionStar rs

> simplifyPattern ps =
>   let reductions = [reduceQuestionStar, reduceMultiStar]
>   in foldl (\p f -> f p) ps reductions

> main :: IO ()
> main = do
>   putStrLn "*** Solution for PatternOptimizer ***"
