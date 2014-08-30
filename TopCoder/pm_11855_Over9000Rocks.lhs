http://community.topcoder.com/stat?c=problem_statement&pm=11855

Problem Statement: Over9000Rocks

You used to have only 30 rocks, but now you have plenty of them. In fact, we
will assume that you have a potentially infinite supply of rocks. You would
like to show that you own over 9000 rocks. You have a set of boxes. You will
choose a subset of those boxes and fill them with rocks so that the total number
of rocks will be over 9000. Each box has a lower and an upper bound on the
number of rocks it may contain.

You are given the int[]s lowerBound and upperBound. For each i, the values
lowerBound[i] and upperBound[i] have the following meaning: If you decide to use
box i, the number of rocks you may put inside the box must be between
lowerBound[i] and upperBound[i], inclusive.

X is a positive integer that has two properties:
X is over 9000.
It is possible to select some of the boxes and fill them with appropriate
numbers of rocks in such a way that the total number of rocks used is exactly X.
Compute and return the number of possible values of X.

> import Data.List (tails, nub)

> combinations 0 _ = [[]]
> combinations n xs = [ y:zs | y:ys <- tails xs
>                            , zs <- combinations (n-1) ys ]

> allCombinations xs = concat [ combinations n xs | n <- [1..length xs] ]

> type Box = (Int, Int)

> boxValues :: [Box] -> [Int]
> boxValues boxes = [slower..supper]
>   where
>     (slower, supper) = foldl (\(sl, su) (l, h) -> (sl+l, su+h)) (0, 0) boxes

> countDistinctValues :: [Box] -> Int
> countDistinctValues boxes =
>   let ps = map boxValues $ allCombinations boxes
>       ds = nub $ concat $ map (filter (>9000)) ps
>   in length ds

> inputToBoxes :: [Int] -> [Int] -> [Box]
> inputToBoxes = zip

> main :: IO ()
> main = do
>   putStrLn "*** Solution to Over9000Rocks ***"
>   answer

> examples = [ ([9000], [9001])
>            , ([9000, 1, 10], [9000, 2, 20])
>            , ([1001, 2001, 3001, 3001], [1003, 2003, 3003, 3003])
>            , ([9000,90000,1,10], [9000,90000,3,15])
>            , ([1,1,1,1,1,1], [3,4,5,6,7,8])
>            ]

> answer = mapM_ (print . go) examples
>   where
>     go (ls, us) = countDistinctValues $ inputToBoxes ls us
