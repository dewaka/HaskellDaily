Simple Word Puzzle solver

> diff [] ys = length ys
> diff xs [] = length xs
> diff (x:xs) (y:ys)
>   | x == y = diff xs ys
>   | otherwise = 1 + diff xs ys

> wordsFrom dict from = filter (\w -> diff from w == 1) dict

> notIn xs = filter (not . (`elem` xs))

The solutions could be repeated in the following function.

> wordPuzzles dict start end = go start []
>   where
>     go s visited = case notIn visited (wordsFrom dict s) of
>                     [] -> []
>                     xs -> if end `elem` xs
>                           then [reverse $ end:visited]
>                           else concat [go s' (s:visited) | s' <- xs]

> exampleDict = ["dog", "cat", "dat", "dag", "cog", "cag"]
