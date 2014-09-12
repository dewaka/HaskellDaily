http://community.topcoder.com/stat?c=problem_statement&pm=11331

You are playing a simple one-player game in which you are given a set of cards.
Each card has a string written on the front and a number on the back. The strings
on all the cards have the same length. You must choose some of these cards (at
least one, possibly all) and place them in a row with the front sides visible,
such that the concatenated string is a palindrome. A palindrome is a string that
reads the same forward and backward. Your score is the sum of the numbers on the
back sides of the chosen cards.

You are given a String[] front and a int[] back describing the set of cards you
are given. The i-th card has front[i] written on the front and back[i] on the
back. Return the maximum possible score you can achieve with these cards.
If it is impossible to compose a palindrome from the given cards, return 0 instead.

{ "topcoder", "redcoder", "redocpot" }
{ 7, 5, 3 }
Returns: 10
You can choose "topcoder" with 7 and "redocpot" with 3 to get a palindrome "topcoderredocpot".

> import Data.List (tails)

> combinations 0 _  = [[]]
> combinations n xs = [ y:zs | y:ys <- tails xs
>                            , zs <- combinations (n-1) ys ]

> selections [] = []
> selections (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- selections xs ]

> permutations [] = [[]]
> permutations xs = [ y:zs | (y, ys) <- selections xs
>                          , zs <- permutations ys ]

> allCombinations xs = concat [ combinations n xs | n <- [1..length xs] ]

> type Card = (String, Int)

> isPalindrome :: Eq b => [b] -> Bool
> isPalindrome xs = all (uncurry (==)) $ zip xs $ reverse xs

> main :: IO ()
> main = do
>   putStrLn "*** Solution for PalindromeGame ***"
>   answer

> answer = undefined
