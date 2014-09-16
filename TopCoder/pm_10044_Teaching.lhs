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

> removePrefix ps xs =
>   let go [] xs = Just xs
>       go (p:ps) (x:xs)
>         | p == x = go ps xs
>         | otherwise = Nothing
>   in case go ps xs of
>       Nothing -> xs
>       Just rs -> rs

> removeCommonPnS word =
>   let w1 = removePrefix "anta" word
>       w2 = reverse $ removePrefix (reverse "tika") (reverse w1)
>   in w2

> uniqueLetters ws = undefined

> main :: IO ()
> main = do
>   putStrLn "*** Solution for Teaching ***"
>   answer

> answer = undefined
