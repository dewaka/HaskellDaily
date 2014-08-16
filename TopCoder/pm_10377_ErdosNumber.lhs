Problem Statement for ErdosNumber

The Erdos number is a way of describing the "collaborative distance" between a
scientist and Paul Erdos by authorship of scientific publications.

Paul Erdos is the only person who has an Erdos number equal to zero. To be
assigned a finite Erdos number, a scientist must publish a paper in
co-authorship with a scientist with a finite Erdos number. The Erdos number of a
scientist is the lowest Erdos number of his coauthors + 1. The order of
publications and numbers assignment doesn't matter, i.e., after each publication
the list of assigned numbers is updated accordingly.

You will be given a String[] publications, each element of which describes the
list of authors of a single publication and is formatted as
"AUTHOR_1 AUTHOR_2 ... AUTHOR_N" (quotes for clarity only). Paul Erdos will be
given as "ERDOS".

Return the list of Erdos numbers which will be assigned to the authors of the
listed publications. Each element of your return should be formatted as
"AUTHOR NUMBER" if AUTHOR can be assigned a finite Erdos number, and just
"AUTHOR" otherwise. The authors in your return must be ordered lexicographically.

Example:
When input is {"KLEITMAN LANDER", "ERDOS KLEITMAN"}
Returns: {"ERDOS 0", "KLEITMAN 1", "LANDER 2" }

> import Data.List (words, nub)
> import Data.Maybe (isJust)

> type Name = String
> type ErNum = Int
> type ErTable = [(Name, ErNum)]

> update k v [] = []
> update k v (e@(x,_):xs)
>   | k == x = (k, v) : xs
>   | otherwise = e : update k v xs

> assignErNum :: ErTable -> [Name] -> ErTable
> assignErNum known authors =
>   let nums = filter isJust $ map (\x -> lookup x known) authors
>
>       Just m = minimum nums
>
>       merge ks [] = ks
>       merge ks (v@(x, n):xs) =
>         case lookup x ks of
>          Nothing -> merge (v:ks) xs
>          Just n' -> if n' <= n then merge ks xs
>                     else merge (update x n ks) xs
>
>   in if nums == [] then known
>      else merge known $ map (\x -> (x, m+1)) authors

> uniqueAuthors alists =
>   let unames = nub $ concat $ map nub alists
>   in unames

> answer submissions =
>   let init = [("ERDOS", 0)]
>       alists = map words submissions
>       uauthors = uniqueAuthors alists
>
>       uniqueAuthors alists =
>         let unames = nub $ concat $ map nub alists
>         in unames
>
>       connect etable = if length etable == length uauthors
>                        then etable
>                        else connect (foldl assignErNum etable alists)
>   in connect init

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ErdosNumber Problem ***"
>   print $ answer ["KLEITMAN LANDER", "ERDOS KLEITMAN", "A ERDOS", "A B", "B C", "C KLEITMAN"]
