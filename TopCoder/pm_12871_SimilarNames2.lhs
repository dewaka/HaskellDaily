Fox Ciel has a list of names on her computer. In this problem, a name is simply
a non-empty string of lowercase letters. All names in her list are distinct.

One day, when she left her seat, she forgot to lock her computer. Then, Lun the
mischievous dog appeared, and randomly shuffled the order of the names in her
list.

Now, Ciel has to restore the original order of names using her memory. You are
given a String[] names and an int L. names contains all names in the shuffled
list in the order they appear. L describes Ciel's memory of the original list.
She remembers that, for each i between 0 and L-2, inclusive, the i-th (0-indexed)
name in the original list was a prefix of the (i+1)-th name.

Let X be the number of possible orders of the names in the original list that
are consistent with Ciel's memory. Calculate and return the value (X modulo
1,000,000,007). X can be 0, which means Ciel's memory is inconsistent with the
names in the list.

Examples:

{"kenta", "kentaro", "ken"}
2
Returns: 3

> select [] = []
> select (x:xs) = (x,xs) : [ (y, x:ys) | (y, ys) <- select xs ]

> permutations [] = [[]]
> permutations xs = [ y:zs | (y, ys) <- select xs
>                          , zs <- permutations ys ]

> isPrefix (x:xs) (y:ys) = x==y && isPrefix xs ys
> isPrefix [] _ = True
> isPrefix _ [] = False

> isPrefixOrder (x:xs@(y:_)) = isPrefix x y && isPrefixOrder xs
> isPrefixOrder [_] = True
> isPrefixOrder [] = True

> hasPrefixOrder n xs = isPrefixOrder xs'
>   where
>     xs' = take n xs

> possibleOrders :: Int -> [String] -> Int
> possibleOrders orderNum names =
>   let ps = filter (hasPrefixOrder orderNum) $ permutations names
>   in length ps

> main :: IO ()
> main = do
>   putStrLn "*** Solution for SimilarNames2 ***"
>   answer

> answer = mapM_ go examples
>   where
>     go (names, num) = print $ possibleOrders num names
>     examples = [ (["kenta", "kentaro", "ken"], 2)
>                , (["hideo", "hideto", "hideki", "hide"], 2)
>                , (["aya", "saku", "emi", "ayane",
>                    "sakura", "emika", "sakurako"], 3)
>                , (["taro", "jiro", "hanako"], 2)
>                ]

TODO:
This last example cannot be done with permutation method as there are going to
be too many permutations to check. Thus, a more mathematical way to calculate
the possible name orders has to be deviced.

> example5 = (["ryota", "ryohei", "ryotaro", "ryo", "ryoga", "ryoma", "ryoko",
>              "ryosuke", "ciel", "lun", "ryuta", "ryuji", "ryuma", "ryujiro",
>              "ryusuke", "ryutaro", "ryu", "ryuhei", "ryuichi", "evima"], 3)
