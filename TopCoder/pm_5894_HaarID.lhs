http://community.topcoder.com/stat?c=problem_statement&pm=5894

Problem Statemnt: HaarID

The Haar wavelet transform is possibly the earliest wavelet transform, introduced
by Haar in 1909. The 1-dimensional version of this transform operates on a
sequence of integer data as follows: First separate the sequence into pairs of
adjacent values, starting with the first and second values, then the third and
fourth values, etc. Next, calculate the sums of each of these pairs, and place
the sums in order into a new sequence. Then, calculate the differences of each
of the pairs (subtract the second value of each pair from the first value), and
append the differences in order to the end of the new sequence. The resulting
sequence is a level-1 transform. Note that this requires the input sequence to
have an even number of elements.

The above describes a level-1 transform. To perform a level-2 transform, we
repeat the above procedure on the first half of the sequence produced by the
level-1 transform. The second half of the sequence remains unchanged from the
previous level. This pattern continues for higher level transforms (i.e., a
level-3 transform operates with the first quarter of the sequence, and so on).
Note that this is always possible when the number of elements is a power of 2.

See the examples for clarification.

Create a class Haar1D with a method transform which takes a int[] data and an
int L as arguments. The output should be a int[] corresponding to the level-L
Haar transform of the data.

> haarTrans' xs = pairWise (+) xs ++ pairWise (-) xs
>   where
>     pairWise _ [] = []
>     pairWise f (x:y:xs) = f x y : pairWise f xs

> haarTransform t xs = transform 1 xs
>   where
>     transform p xs
>       | p > t = xs
>       | otherwise =
>           let n = length xs `div` 2^(p-1)
>               (ts, rs) = (take n xs, drop n xs)
>           in transform (p+1) (haarTrans' ts ++ rs)

> answer = mapM_ (print . go) examples
>   where
>     go (xs, n) = haarTransform n xs
>     examples = [ ([1, 2, 3, 5], 1)
>                , ([1, 2, 3, 5], 2)
>                , ([1, 2, 3, 4, 4, 3, 2, 1], 3)
>                , ([94, 47, 46, 28, 39, 89, 75, 4, 28, 62, 69, 89, 34, 55, 81, 24], 2)
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to HaarID"
>   answer
