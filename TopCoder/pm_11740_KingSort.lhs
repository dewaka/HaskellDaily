http://community.topcoder.com/stat?c=problem_statement&pm=11740

Problem Statement: KingSort

Every good encyclopedia has an index. The entries in the index are usually
sorted in alphabetic order. However, there are some notable exceptions. In this
task we will consider one such exception: the names of kings.

In many countries it was common that kings of the same name received ordinal
numbers. This ordinal number was written as a Roman numeral and appended to the
actual name of the king. For example, "Louis XIII" (read: Louis the thirteenth)
was the thirteenth king of France having the actual name Louis.

In the index of an encyclopedia, kings who share the same name have to be sorted
according to their ordinal numbers. For example, Louis the 9th should be listed
after Louis the 8th.

You are given a String[] kings. Each element of kings is the name of one king.
The name of each king consists of his actual name, a single space, and a Roman
numeral. Return a String[] containing the names rearranged into their proper
order: that is, the kings have to be in ascending lexicographic order according
to their actual name, and kings with the same name have to be in the correct
numerical order.

We need a function to convert from Roman numerals.


> fromRoman :: String -> Int
> fromRoman = undefined

> romanSymbol :: Char -> Int
> romanSymbol 'I' = 1
> romanSymbol 'V' = 5
> romanSymbol 'X' = 10
> romanSymbol 'L' = 50
> romanSymbol 'C' = 100
> romanSymbol 'D' = 500
> romanSymbol 'M' = 1000

> main :: IO ()
> main = do
>   putStrLn "*** Solution to KingSort ***"
