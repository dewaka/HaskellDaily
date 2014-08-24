http://community.topcoder.com/stat?c=problem_statement&pm=10369

Problem Statement: TheSwap

There is nothing more beautiful than just an integer number.

You are given an integer n. Write down n in decimal notation with no leading
zeroes, and let M be the number of written digits. Perform the following
operation exactly k times:

Choose two different 1-based positions, i and j, such that 1 <= i < j <= M.
Swap the digits at positions i and j. This swap must not cause the resulting
number to have a leading zero, i.e., if the digit at position j is zero,
then i must be strictly greater than 1.
Return the maximal possible number you can get at the end of this procedure.
If it's not possible to perform k operations, return -1 instead.

> import Data.List (tails)

We need to select 2 indices to swap.

> combinations 0 _ = [[]]
> combinations n xs = [ y:zs | y:ys <- tails xs
>                            , zs <- combinations (n-1) ys]

> swap i j xs =
>   let (n, x, y) = (length xs, xs !! i, xs !! j)
>       fs = take i xs
>       ms = drop (i+1) $ take j xs
>       ls = drop (j+1) xs
>   in concat [fs, [y], ms, [x], ls] -- swap positions of x and y

> swappedNums :: Int -> [Int]
> swappedNums num = [ read (swap i j snum)
>                   | [i,j] <- combinations 2 [0..length snum-1]]
>   where
>     snum = show num

> largestAfterSwaps n num =
>   let swapTimes n xs
>         | n > 0 = swapTimes (n-1) $ concatMap swappedNums xs
>         | otherwise = xs
>       ys = swapTimes (n-1) $ swappedNums num
>   in if null ys then Nothing  -- Possible swaps could be null
>      else Just $ maximum ys

> largestAfterSwaps' n num =
>   case largestAfterSwaps n num of
>    Nothing -> -1
>    Just c ->
>      -- We have to eliminate numbers which were read in with a zero
>      -- in the beginning position. We can do that by the fact that their
>      -- string length will be lower than the original one.
>      let [sc, sn] = map (length . show) [c, num]
>      in if sc < sn then -1
>         else c

> answer = mapM_ (print . uncurry largestAfterSwaps') examples
>   where
>     examples = [ (1, 16375)
>                , (1, 432)
>                , (4, 90)
>                , (2, 5)
>                , (2, 436659)
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TheSwap"
>   answer
