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

> import Data.List (nub, tails, sort, sortBy)

> combinations 0 _ = [[]]
> combinations n xs = [ y:zs | y:ys <- tails xs
>                            , zs <- combinations (n-1) ys ]

> canFormWord alphabet word = all (\t -> t `elem` alphabet) word

> maximumWordsPossible limit words =
>   let fullAlphabet = nub $ concat words
>       allPossible = [ length $ filter (canFormWord c) words |
>                       c <- combinations limit fullAlphabet ]
>   in case reverse $ sort allPossible of
>       [] -> 0
>       (m:_) -> m

> simplifyWords words = map simplify words
>   where
>     simplify word =
>       foldl (\acc c -> if c `elem` "antic" then acc else c:acc) "" word

This simplifies the combinations we have to try using the domain knowledge
that all the words should have the 'anta' prefix and 'tica' suffix.

> maxPossibleWords :: Int -> [String] -> Int
> maxPossibleWords limit words =
>   let swords = simplifyWords words
>       limit' = limit - 5
>   in if limit' < 0 then 0
>      else maximumWordsPossible limit' swords

> main :: IO ()
> main = do
>   putStrLn "*** Solution for Teaching ***"
>   answer

> answer = mapM_ go examples
>   where
>     go (words, num) = print $ maxPossibleWords num words
>     examples = [ (["antarctica","antahellotica","antacartica"], 6)
>                , (["antaxxxxxxxtica","antarctica"], 3)
>                , (["antabtica","antaxtica","antadtica","antaetica", "antaftica",
>                    "antagtica","antahtica","antajtica","antaktica"], 8) ]
