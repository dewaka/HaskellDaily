http://community.topcoder.com/stat?c=problem_statement&pm=12107

You have a int[] A with N elements.

Your goal is to change it into a int[] that contains each number from 1 to N
exactly once. The change will consist of zero or more steps. In each step, you
may pick an arbitrary element of A and increment its value by k.
You may pick the same element multiple times. Note that you are not allowed to
decrement the value of any element.

You are given the int k and the int[] A. Return "POSSIBLE" if it is possible to
achieve your goal. Return "IMPOSSIBLE" otherwise.

Examples

0)
3
{1,2,4,3}
Returns: "POSSIBLE"
This sequence of length 4 already contains all numbers from 1 to 4 exactly once.
Note that their order does not matter.

1)
5
{2,2}
Returns: "IMPOSSIBLE"

2)
1
{1,1,1,1,1,1,1,1}
Returns: "POSSIBLE"
There are many ways to achieve the goal. For example, it is possible to obtain
the sequence {1,2,3,4,5,6,7,8}. To do this, just increment the element at each
position one by one until it reaches the required value.

3)
2
{5,3,3,2,1}
Returns: "IMPOSSIBLE"
We want to have the values {1,2,3,4,5}, in any order. Currently, we are missing
the 4. As k=2, the only way to produce a 4 is by incrementing a 2.
But if we increment our only 2, we will have no way of producing another 2.

4)
9
{1,2,3,1,4,5,6,7,9,8}
Returns: "POSSIBLE"

5)
2
{1,1,1,1,1,1,2,2,2,2,2,2}
Returns: "POSSIBLE"

6)
1
{1}
Returns: "POSSIBLE"

> import Data.List (sort)

> conformingSequence start xs = all (\(a, b) -> a==b) $ zip xs [start..]

> select x xs = go x xs []
>   where
>     go _ [] _ = Nothing
>     go x (y:ys) acc
>       | x == y = Just (x, acc++ys)
>       | otherwise = go x ys (acc++[y])

> canChange' _ [] _ = True
> canChange' n xs s = possibleWith s || possibleWith (s-n)
>   where
>     possibleWith t =
>       case select t xs of
>         Nothing -> False
>         Just (_, ys) -> canChange' n ys (t+1)

> canChange n xs =
>   let xs' = sort xs
>   in conformingSequence 1 xs' || canChange' n xs' 1

> answer = mapM_ (print . go) examples
>   where
>     go (n, xs) = if canChange n xs then "POSSIBLE" else "IMPOSSIBLE"
>     examples = [ (3, [1,2,3,4])
>                , (5, [2,2])
>                , (1, [1,1,1,1,1,1,1,1])
>                , (2, [5,3,3,2,1])
>                , (9, [1,2,3,1,4,5,6,7,9,8])
>                , (2, [1,1,1,1,1,1,2,2,2,2,2,2])
>                , (1, [1])
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to IncrementingSequence ***"
>   answer
