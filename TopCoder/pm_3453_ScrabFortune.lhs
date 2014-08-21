You are playing a game that borrows principles from the popular games Scrabble
and Wheel of Fortune. The game board consists of one or more words, all of
which are completely hidden at the very beginning. You have a pool of letters,
and you select one letter at a time. Each time you select a letter, all
instances of that letter will be revealed on the board. The goal of the game
is to accurately guess all the words.

You are given a String pool and a String[] board. pool contains all the letters
in your pool and board contains all the words on the board. You are also given
an int threshold. When the total number of hidden letters on the board is less
than or equal to threshold, you will be able to accurately guess all the words
and win. The total number of hidden letters includes all occurrences of letters,
not just distinct letters. For example, if the hidden letters are "aaab", the
total number of hidden letters is 4.

Your goal is to win the game by selecting the minimum possible number of letters
from your pool. Return a String containing the letters you should choose
(all lowercase). If there are multiple solutions, return the one that comes
first alphabetically. If it is impossible to win,
return "IMPOSSIBLE" (quotes for clarity).

> import Data.List (nub, tails, sort)

We have to try multiple combinations and test the minimal we can achieve. If the
minimum we get is less than or equal to the allowed amount then we have a win.

> countKnown ks (x:xs) = (if x `elem` ks then 1 else 0) + countKnown ks xs
> countKnown _ _ = 0

> countUnknown ks xs = length xs - countKnown ks xs

> countCumulativeUnknown ks words = sum $ map (countUnknown ks) words

Given suggestion is a winning suggestion if the cumulative uknowns from words
is less than or equal to the threshold value.

> isWinning threshold suggestion words =
>   threshold >= countCumulativeUnknown suggestion words

We need a function to select given number of characters from the pool.
Another requirement for this problem is that the selected characters should be
distinct. That part is easy to satisfy by selecting from a distinct pool
rather than from the original pool (using nub function from Data.List).

Select is the same as combinations which we can define using list comprehensions
as follows.

> combinations 0 _ = [[]]
> combinations n xs = [y:ys | y:xs' <- tails xs
>                           , ys <- combinations (n-1) xs']

Thus we have,

> select = combinations

Now let's assume that our select function works and that it can actually select
the number of characters we want. Let's say our distinct pool is pool' and
L = length pool', then [select i pool' | i <- [0..L]] is the all the selections
possible. The selections will be from [[]] to [pool'] and we got to find the
first one which contains a selection such that it will be a winning selection.
If there is no such winning selection it is an "IMPOSSIBLE" one.
To get the lexicographically smallest answers first we will sort the pool' as
well before trying finding answers.

> winningSelections threshold pool words =
>   let pool' = sort $ nub pool
>       ws = [p | i <- [0..length pool']
>               , p <- select i pool'
>               , isWinning threshold p words]
>   in if null ws then "IMPOSSIBLE"
>      else head ws

> exampleWords1 = ["scrab", "fortune", "is", "fun"]
> exampleSuggestion1 = "abcfs"

> examples = [ (10, "fpsctyba", ["scrab", "fortune", "is", "fun"])
>            , (6, "llllo", ["hello", "world"])
>            , (16, "l", ["blbvbvb", "ajllkjk", "bjkdfle"])
>            , (3, "abc", ["aabbccc"])
>            , (600, "tkse", ["easy", "to", "solve"])
>            ]

> answer = mapM_ (print . ws) examples
>   where
>     ws (t, p, w) = winningSelections t p w

> main :: IO ()
> main = do
>   putStrLn "*** Solution of ScrabFortune ***"
>   answer
