Problem Statement for EllysScrabble


Elly is playing Scrabble with her family. The exact rules of the game are not
important for this problem. You only need to know that Elly has a holder that
contains a row of N tiles, and that there is a single letter on each of those
tiles. (Tiles are small square pieces of wood. A holder is a tiny wooden shelf
with room for precisely N tiles placed in a row.)

While Elly waits for the other players, she entertains herself in the following
way. She slightly taps the table, causing the tiles on her holder jump a little
and some of them switch places. Formally, suppose that Elly has N tiles. There
are N positions on the holder, we will label them 0 through N-1 from left to
right. When Elly taps the table, the tiles on her holder will form some
permutation of their original order. You are given an int maxDistance with the
following meaning: in the permutation that Elly produces by tapping the table,
no tile will be more than maxDistance positions away from its original
position (in either direction).

> partitions xs = go xs []
>   where
>     go [] ts = [(ts, [])]
>     go xs@(x:xs') ts = (ts, xs) : go xs' (ts++[x])

> permutations [] = [[]]
> permutations (x:xs) = [(h++[x]++t) | p <- permutations xs, (h,t) <- partitions p]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to EllysScrabble ***"
