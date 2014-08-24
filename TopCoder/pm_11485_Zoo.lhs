http://community.topcoder.com/stat?c=problem_statement&pm=11485

Problem Statement: Zoo

There are N animals numbered 0 to N-1 in a zoo. Each animal is a rabbit or a cat.
Their heights are pairwise distinct.

Fox Jiro can't distinguish between rabbits and cats, so he asked the following
question to each animal: "How many animals of the same kind as you are taller
than you?" Each rabbit tells the number of rabbits taller than him, and each cat
tells the number of cats taller than her. The differences of heights are slight,
so Fox Jiro can't tell which animals are taller than other animals. However,
each animal is able to determine which animals are taller that him and which
ones are shorter.

The answer given by the i-th animal is answers[i]. Given these numbers, return
the number of configurations resulting in exactly those numbers, assuming
everyone tells the truth. Two configurations are different if there exists an
i such that the i-th animal is a rabbit in one configuration and cat in the
other configuration.

This is a very interesting problem. I think at the heart of this problem the
actual question we are being asked is how many ways we can divide a given
list of integers into two sets such that no elements will be reapting within
the set and also such that those two sets will contain all the numbers in the
original list.
This question is a more mathematically well defined one we can answer.

This function will split a given list to two lists ensuring that there are
no repeating elements in sublists. Furthermore if successful the first element
of the tuple will contain the larger list.

> addOrdered x [] = [x]
> addOrdered x ys@(y:ys')
>   | x > y = y : addOrdered x ys'
>   | otherwise = x:ys

> splitDistinct xs = go xs [] []
>   where
>     go [] fs ls = Just (fs, ls)
>     go (x:xs) fs ls =
>       if x `elem` fs
>       then if x `elem` ls
>            then Nothing
>            else go xs fs (x `addOrdered` ls)
>       else go xs (x `addOrdered` fs) ls

> splitToTwoSets xs =
>   case splitDistinct xs of
>    Nothing -> Nothing
>    s@(Just (rs, cs)) ->
>      if valid rs && valid cs
>      then s
>      else Nothing
>     where
>       valid xs = xs == [0..length xs-1]

> possibleConfigurations xs =
>   case splitToTwoSets xs of
>    Nothing -> 0
>    Just (rs, cs) ->
>      if null cs then 2
>      else
>        let commonCount = length cs
>        in if length rs == length cs
>           then commonCount * 2  -- They are symmetric
>           else commonCount * 2 * 2 -- In asymmetric case possible configurations are doubled

> answer = mapM_ (print . possibleConfigurations) examples
>   where
>     examples = [ [0, 1, 2, 3, 4]
>                , [5, 8]
>                , [0, 0, 0, 0, 0, 0]
>                , [1, 0, 2, 0, 1]
>                , [1, 0, 1]
>                -- My own additions
>                , [0, 0]       -- [R0, C0], [C0, R0] thus 2
>                , [0, 1, 0]    -- [R0, R1, C0], [C0, R1, R0], [C0, C1, R0], [R0, C1, C0] thus 4
>                , [0, 1, 1, 0] -- Similar to above case but there will be another R or C
>                , [0, 3, 1, 0] -- Invalid case, thus 0
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TheSwap"
>   answer
