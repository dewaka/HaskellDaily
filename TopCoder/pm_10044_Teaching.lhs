A language teacher in Antarctica wants his students to be able to read as many
words as possible. However, he only has time to teach them K letters. After
that, the students will only be able to read words containing only those K
letters. Your task is to determine which K letters should be taught to maximize
the number of words that the students will be able to read.

The Antarctican language is famous because it only contains words that start
with "anta" and end with "tica" (quotes for clarity only). You are given a
String[] words containing all the words in the language.
Return the maximum number of words the students will be able to read if they
learn the optimal set of K letters.

> import Data.List (nub, tails)

> combinations 0 _ = [[]]
> combinations n xs = [ y:zs | y:ys <- tails xs
>                            , zs <- combinations (n-1) ys ]

This function returns letter frequencies in terms of occurances within words but
discounting multiple occurances within the same word.

> letterFreqencies :: [String] -> [(Char, Int)]
> letterFreqencies words =
>   let letters = nub $ concat words
>       countOccurances c = sum $ map (\w -> if c `elem` w then 1 else 0) words
>       ftable = foldl (\acc c -> (c, countOccurances c):acc) [] letters
>   in ftable

>

> main :: IO ()
> main = do
>   putStrLn "*** Solution for Teaching ***"
>   answer

> answer = undefined
