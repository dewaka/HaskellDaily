-- http://community.topcoder.com/stat?c=problem_statement&pm=13062

{-
Problem Statement: FizzBuzzTurbo

Fizz Buzz is a simple game used to teach kids about divisibility.
The goal of the game is to say positive integers in increasing order, with a
twist: You don't say the numbers divisible by 3 and 5. Instead, whenever a
number was divisible by 3 you say "fizz" and for a number divisible by 5 you
say "buzz". (Thus, if a number was divisible by 15, you say "fizzbuzz".)

Here is how the game starts: 1, 2, fizz, 4, buzz, fizz, 7, 8, fizz, buzz, 11,
fizz, 13, 14, fizzbuzz, 16, 17, fizz, 19, buzz, fizz, 22, 23, fizz, buzz, 26,
]fizz, 28, 29, fizzbuzz, 31, 32, fizz, 34, buzz, fizz, ...

Fizz Buzz has also become a traditional programming interview question.
However, in this problem we have a more tricky assignment for you.

You are given longs A and B. Consider the part of the game that corresponds to
integers from A to B, inclusive. During this part of the game, you will
say "fizz" X times, "buzz" Y times, and "fizzbuzz" Z times. Return a long[] with
three elements: {X,Y,Z}.

-}

-- This is the traditional fizz buzz function generating infinite
-- stream of fizzes and buzzes!
fizzBuzz :: [String]
fizzBuzz = [fb n | n <- [1..]]
  where
    fb n
      | n `rem` 3 == 0 && n `rem` 5 == 0 = "fizzbuzz"
      | n `rem` 3 == 0 = "fizz"
      | n `rem` 5 == 0 = "buzz"
      | otherwise = show n

fizzBuzzCounters a b =
  let subseq = take (b+1-a) $ drop (a-1) fizzBuzz
      count x = length $ filter (==x) subseq
  in (count "fizz", count "buzz", count "fizzbuzz")

main :: IO ()
main = do
  putStrLn "*** FizzBuzzTurbo Problem ***"
