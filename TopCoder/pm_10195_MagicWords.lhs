http://community.topcoder.com/stat?c=problem_statement&pm=10195

Consider a string T containing exactly L characters. The string T(i) is a cyclic
shift of T starting from position i (0 <= i < L). T(i) contains exactly the same
number of characters as T. For each j between 0 and L-1, inclusive, character j
of T(i) is equal to character (i+j)%L of T. Let's call T a magic word if there
are exactly K positions i such that T(i) = T.

You are given a String[] S containing exactly N words. For each permutation
p = (p[0], p[1], ..., p[N-1]) of integers between 0 and N-1, inclusive, we can
define a string generated by this permutation as a concatenation
S[p[0]] + S[p[1]] + ... + S[p[N-1]]. Return the number of permutations that
generate magic words. All indices in this problem as 0-based.

{"CAD","ABRA","ABRA"}
1
Returns: 6
Every permutation generates a magic word here.

{"AB","RAAB","RA"}
2
Returns: 3
The magic words are "ABRAABRA" and "RAABRAAB". The first word is generated only
by the permutation (0, 1, 2), and the second word is generated by the two
permutations (1, 2, 0) and (2, 0, 1).

Defining permutations function using (now classic for me) selections function.

> selections [] = []
> selections (x:xs) = (x,xs) : [(t, x:ts) | (t, ts) <- selections xs]

> permutations [] = [[]]
> permutations xs = [ t:zs | (t, ts) <- selections xs
>                          , zs <- permutations ts ]

> cyclicShift i xs = drop i xs ++ take i xs

> cyclicShifts xs = [cyclicShift i xs | i <- [0..length xs-1]]

> eqCycleCount xs = length [cs | cs <- cyclicShifts xs, cs == xs]

We want to make checking whether a given list is a magic list as general
as possible, not just for Strings.

> isMagic n xs = n == eqCycleCount xs

> allWords = map concat . permutations

> magicWords n ws = filter (isMagic n) $ allWords ws

> answer = mapM_ (print . go) examples
>   where
>     go (n, xs) = length $ magicWords n xs
>     examples = [ (1, ["CAD","ABRA","ABRA"])
>                , (2, ["AB","RAAB","RA"])
>                , (1, ["AA","AA","AAA","A"])
>                , (15, ["AA","AA","AAA","A","AAA","AAAA"])
>                , (3, ["ABC","AB","ABC","CA"])
>                , (1, ["A","B","C","A","B","C"])
>                , (1, [ "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAA"
>                      , "AAAAAAAAAAAAAAAAAAAB"])
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TheSwap"
>   answer
