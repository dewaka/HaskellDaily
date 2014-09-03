http://community.topcoder.com/stat?c=problem_statement&pm=12025

For a given string S of length n an inversion is a pair of integers (i, j) such
that 0 <= i < j <= n-1 and S[i] > S[j]. (That is, the character at 0-based index
i is greater than the character at 0-based index j.) For example, the string
"abcab" has 3 inversions: (1, 3), (2, 3), and (2, 4).

Given are ints n and minInv, and a String minStr. We will consider all strings
that are permutations of the first n lowercase English letters. That is, these
strings have length n and contain each of the first n letters exactly once.
Out of these strings, return the lexicographically smallest string R with the
following two properties:

The number of inversions in R is at least minInv.
The string R is not lexicographically smaller than minStr.
If there is no such string, return an empty String instead.

> import Data.List (tails)

> -- | Following function returns possible inversions for a given ordered list
> possibleInversions :: Ord a => [a] -> [(Int, Int)]
> possibleInversions xs = concat $ go $ zip [0..] xs
>   where
>     go ((i, c):xs) = inversions : go xs
>       where
>         inversions = map (\(j, _) -> (i, j)) $ filter ((<c) . snd) xs
>     go [] = []


> selections [] = []
> selections (x:xs) = (x, xs) : [(y,x:ys) | (y,ys) <- selections xs]

> permutations [] = [[]]
> permutations xs = [ y:zs | (y,ys) <- selections xs
>                          , zs <- permutations ys ]

> stringsOfLen n = permutations $ take n ['a'..]

> computeStrings len minInv compStr = [s | s <- stringsOfLen len
>                                        , s > compStr
>                                        , inversionCount s >= minInv ]
>   where
>     inversionCount = length . possibleInversions

> main :: IO ()
> main = do
>   putStrLn "*** Solution to StrIIRec ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go (len, minInv, str) =
>       case computeStrings len minInv str of
>         [] -> ""
>         (h:_) -> h
>     examples = [ (2, 1, "ab")
>                , (9, 1, "efcdgab")
>                , (11, 55, "debgikjfc")
>                , (15, 0, "e")
>                , (9, 20, "fcdebiha")
>                ]
