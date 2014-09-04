http://community.topcoder.com/stat?c=problem_statement&pm=3075

Problem Statement: HiddenNumbers

You are part of a data-mining operation that only cares about numbers as data. As such,
you have been assigned to write a program that gets a long chunk of text and searches for
all the numbers in the text. Because your boss cares more about large numbers, he only
wants you to give him the larger half of the numbers.

You are given a String[] text, the lines of text to be searched for numbers, and you are
to find and return the larger half of the numeric substrings in text. Numeric substrings
should never overlap, and you should always use the longest possible contiguous sequence
of numbers. For instance, "sk12345fj" has just one numeric substring - "12345".
"sk12 345fj" has 2 - "12" and "345". If there are an odd number of numeric substrings in
text, you will return (n+1)/2 strings. These numbers should be sorted in ascending order
of numeric value, and returned with any leading zeros intact. If two numbers found in
text have the same numeric value but have different numbers of leading zeros, the one with
fewer leading zeros should be considered "less". It is possible for numbers to wrap across
lines - if one line ends in a number and the next one begins with a number, these are
consecutive parts of the same number.

Constraints
- text will have between 0 and 50 elements.
- Each element in text will have between 0 and 50 characters.
- Each element of text will contain only digits ('0'-'9'), letters ('a'-'z', 'A'-'Z'),
  and spaces (' ').
- All of the numbers hidden in text will be between 0 and 263-1, but they may
  have leading zeros.

Examples
0)
{"098m03r9f80239802389f0m9KDKLKLJDKLJm0983m890DMOm03",
 "dlkfj3hljf4h3klhl  4j4 444 44  rjhkrrkr34534539893",
 " 390804980498409480 dkldjkl djkl djkl d00000002998"}
Returns:
{ "9",
 "44",
 "098",
 "444",
 "890",
 "0983",
 "00000002998",
 "34534539893",
 "80239802389",
 "390804980498409480" }
Most of the omitted numbers are one-digit numbers.

1)
{"39 000220 30 skldjdije939939slkk 3090 2912kjdk3949",
 "dlkjd dkljsl098 dkd3 23kdkdkl 0000002222kdjdie9000"}
Returns: { "0000002222",  "2912",  "3090",  "3949",  "9000",  "939939" }
Be careful about using the length of the string to compare the numeric values - leading zeros can mess you up!

2)
{}
Returns: { }
This is a shorter one.

3)
{"0022 22k00022a022"}
Returns: { "0022",  "00022" }

> import Data.Char (isDigit)
> import Data.List (sortBy)

> sortNumbers :: [String] -> [String]
> sortNumbers = sortBy numOrder
>   where
>     numOrder sn1 sn2 =
>       let n1 :: Int
>           n1 = read sn1
>           n2 :: Int
>           n2 = read sn2
>       in case n1 `compare` n2 of
>            EQ -> sn1 `compare` sn2
>            e -> e

> extractNumbers [] = []
> extractNumbers xs@(x:ys)
>   | isDigit x = num : extractNumbers xs'
>   | otherwise = extractNumbers ys
>   where
>     num = takeWhile isDigit xs
>     xs' = dropWhile isDigit xs

> mineNumbers xs =
>   let nums = sortNumbers $ extractNumbers xs
>       count = floor $ (fromIntegral $ length nums) / 2.0
>   in drop count nums

> main :: IO ()
> main = do
>   putStrLn "*** Solution to HiddenNumbers ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go strs = mineNumbers $ concat strs
>     examples = [ [ "098m03r9f80239802389f0m9KDKLKLJDKLJm0983m890DMOm03"
>                  , "dlkfj3hljf4h3klhl  4j4 444 44  rjhkrrkr34534539893"
>                  , " 390804980498409480 dkldjkl djkl djkl d00000002998" ]
>                , [ "39 000220 30 skldjdije939939slkk 3090 2912kjdk3949"
>                  , "dlkjd dkljsl098 dkd3 23kdkdkl 0000002222kdjdie9000" ]
>                , [  ]
>                , [ "0022 22k00022a022" ]
>                ]

> example1 = "39 000220 30 skldjdije939939slkk 3090 2912kjdk3949dlkjd dkljsl098 dkd3 23kdkdkl 0000002222kdjdie9000"
