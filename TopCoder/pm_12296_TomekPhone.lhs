> import Data.List (sort)


> calculate :: Int -> Int -> [Int] -> Int
> calculate num pnum fs =
>   let sfs = reverse $ sort fs -- Decending order
>       buttonPresses num pnum pos = pos `div` pnum + pos `rem` num
>   in sum [n * (buttonPresses num pnum p) | (n, p) <- zip sfs [1..]]

> calcInputs numButtons perButton freqs =
>   if numButtons * perButton < length freqs
>   then -1
>   else calculate numButtons perButton freqs

> calculateBtn fqs bts =
>   let sfqs = reverse $ sort fqs
>   in undefined

> calcBasedOnButtons fqs bts =
>   if length fqs > sum bts
>   then -1
>   else calculateBtn fqs bts

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TomekPhone ***"
>   answer

> answer = undefined -- mapM_ (print . go) examples
>   where
>     -- go (fs, (nb, pb)) = calcInputs nb pb fs
>     go = undefined
>     examples = [ ([7,3,4,1], [2, 2])
>                , ([13,7,4,20], [2, 1])
>                , ([11,23,4,50,1000,7,18], [3,1,4])
>                ]
