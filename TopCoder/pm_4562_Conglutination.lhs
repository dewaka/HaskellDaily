http://community.topcoder.com/stat?c=problem_statement&pm=4562

Problem Statement: Conglutination

You are developing a new software calculator. Some people use the calculator to
check the sums of several numbers, but they sometimes get unexpected results
because they forget to press the 'plus' button. You have almost solved this
problem, but a small method is still required.

You will be given a String conglutination and an int expectation.
Your method should split conglutination into two numbers A and B so that
A + B = expectation.
Return the result as a String in the form "A+B" (quotes for clarity only).
A and B must contain at least one digit each.
Leading zeros are allowed and they must be preserved in the result.
If there are several possible splits, choose the one with the smallest value
of A. Return an empty String if there are no possible splits.

Examples:

1) "22", 4
   Returns: "2+2"

> conglutination [] _ = Nothing
> conglutination (x:xs) num = go [x] xs
>   where
>     go ps ts@(y:ys) =
>       let p :: Int
>           t :: Int
>           (p, t) = (read ps, read ts)
>       in if p+t == num then Just (ps, ts)
>          else go (ps++[y]) ys
>     go _ [] = Nothing

> conglutination' xs num = case conglutination xs num of
>                           Nothing -> ""
>                           Just (ps, ts) -> ps ++ "+" ++ ts

> main :: IO ()
> main = do
>   putStrLn "*** Solution to Conglutination ***"
>   answer

> answer = mapM_ go examples
>   where
>     go (xs, num) = putStrLn $ conglutination' xs num
>     examples = [ ("22", 4)
>                , ("536", 41)
>                , ("123456000789", 1235349)
>                , ("123456789", 4245)
>                , ("112", 13)
>                ]
