http://community.topcoder.com/stat?c=problem_statement&pm=5984

As part of a larger scale project, you need to write a component which generates
consecutive positive integers. Only certain digits may appear in the input and
in the integers generated, and leading zeros aren't allowed.

You are given a int[] allowed containing the list of allowed digits, and a
String current representing the current integer. Return a String representing
the first integer larger than current composed only of digits in allowed.

If current represents an invalid integer according to the first paragraph,
return "INVALID INPUT" (quotes for clarity).

> import Data.List (sort, tails)

> isValid pool num = all (`elem` pool) $ show num

Combinations algorithm is not required for this problem. But deveveloped here
to understand the connection between the one with replaements.

> combinations 0 _ = [[]]
> combinations n xs = [ (y:zs) | (y:ys) <- tails xs
>                              , zs <- combinations (n-1) ys ]

> combinationsWithReplacements 0 _ = [[]]
> combinationsWithReplacements  n xs =
>   [ (y:zs) | (y:_) <- tails xs
>            , zs <- combinationsWithReplacements (n-1) xs ]

> nextNumber :: String -> Int -> Maybe Int
> nextNumber pool num = if isValid pool num
>                       then calcNext
>                       else Nothing
>   where
>     calcNext = let snum = show num
>                    n = length snum
>                    -- We have to sort the pool in asc order so that we get
>                    -- consecutive numbers through the combinations algorithm
>                    spool = sort pool
>                    npool = combinationsWithReplacements n spool ++
>                            combinationsWithReplacements (n+1) spool
>                    next = filter ((>num) . read) npool
>                in if null next then Nothing
>                   else Just (read $ head next)

> main :: IO ()
> main = do
>   putStrLn "*** Solution to PointyWizardHats ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go (npool, snum) = case snum of
>                         ('0':_) -> "INVALID INPUT"
>                         _ -> case nextNumber pool num of
>                               Nothing -> "INVALID INPUT"
>                               Just num -> show num
>       where
>         num = read snum
>         pool = foldl (++) [] $ map show npool
>     examples = [ ([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ], "16")
>                , ([ 0, 1, 2, 3, 4, 5, 6, 8, 9 ], "16")
>                , ([ 3, 5, 8 ], "548")
>                , ([ 5, 3, 4 ], "033")
>                , ([ 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ], "999")
>                , ([ 0, 1, 2, 3, 4, 5 ], "0")
>                , ([ 1 ], "1")
>                ]
