http://community.topcoder.com/stat?c=problem_statement&pm=5945

Problem Statement: VolumeDiscount

When a customer buys large quantities of a product, frequently the seller will
offer a volume discount. For instance, one unit might cost 10 dollars, but might
be offered in packages of 5 for 45 dollars. In such a case, it always makes sense
buy the bulk lots to save money. In some other cases, however, it might not always
make sense. Suppose a single unit were on sale for 8 dollars. In such a case,
purchasing single units would be less expensive than purchasing a 5-pack.

You are given a String[] priceList describing the number of units available in
each bundle, and the cost of the bundle. Each element is of the form
"units cost" (quotes added for clarity). You are also given an int quantity,
the number of units you wish to purchase.

Return an int indicating the best possible cost to purchase at least the desired
quantity of units.

Example 1:
Bundles {"1 10" "5 45"}
Amount to buy: 10

Bundles {"99 913", "97 173", "50 464", "80 565"}
Amount to buy: 18
Answer: 173. Here we have no choice but to buy the bundle with the cheapest
        as all the bundles are actually larger than the amount we have to buy

> import Data.List (sortBy)

> type Bundle = (Int, Int)

> parseBundle :: String -> Bundle
> parseBundle str =
>   let qs = takeWhile (/= ' ') str
>       (' ':bs) = dropWhile (/= ' ') str
>   in (read qs, read bs)

> bundleCompare :: Bundle -> Bundle -> Ordering
> bundleCompare (p1, b1) (p2, b2) = c1 `compare` c2
>   where
>     c1 = fromIntegral b1 / fromIntegral p1
>     c2 = fromIntegral b2 / fromIntegral p2

> bundleSort :: [Bundle] -> [Bundle]
> bundleSort = sortBy bundleCompare

> findBestPrice q sbs =
>   let bs = bundleSort $ map parseBundle sbs
>   in if null bs
>      then error "Cannot allocate with empty bundles"
>      else if all ((>q) . fst) bs
>           then snd $ head bs
>           else calcBestPrice q bs
>   where
>     calcBestPrice q bs
>       | q <= 0 = 0
>       | otherwise = p + calcBestPrice q' bs
>       where
>         -- Does this assumption hold in practice all the time?
>         ((b, bp):_) = dropWhile ((>q) . fst) bs
>         p = bp * (q `div` b)
>         q' = q `rem` b

> answer = mapM_ (print . uncurry findBestPrice) examples
>   where
>     examples = [ (10, ["1 10","5 45"])
>                , (10, ["1 8", "5 45"])
>                , (18, ["99 913", "97 173", "50 464", "80 565"])
>                , (81, ["2 272","1 166","10 993"])
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to VolumeDiscount ***"
>   answer
