http://community.topcoder.com/stat?c=problem_statement&pm=1704

Let's say you have a binary string such as the following:

011100011

One way to encrypt this string is to add to each digit the sum of its adjacent
digits. For example, the above string would become:

123210122

In particular, if P is the original string, and Q is the encrypted string,
then Q[i] = P[i-1] + P[i] + P[i+1] for all digit positions i. Characters off
the left and right edges of the string are treated as zeroes.

> import Data.Char (ord, chr)

> encode :: [Int] -> [Int]
> encode nums = go (0:nums)
>   where
>     go (x:xs@(y:z:_)) = x+y+z : go xs
>     go [x,y] = [x+y]
>     go _ = []

> decode :: [Int] -> (Maybe [Int], Maybe [Int])
> decode ys = (decodeWith 0, decodeWith 1)
>   where
>     decodeWith n =
>       let ds = n : go ys (0, n)
>       in if all (<=1) ds then Just ds else Nothing
>     go [_] _ = []
>     go (x:xs) (p1, p2) = p' : go xs (p2, p')
>       where
>         p' = x - p1 - p2
>     go [] _ = []

> toBinString :: [Int] -> String
> toBinString = map $ \n -> case n of
>                            1 -> '1'
>                            0 -> '0'

> fromBinString :: String -> [Int]
> fromBinString = map $ \c -> case c of
>                              '1' -> 1
>                              '0' -> 0

> main :: IO ()
> main = do
>   putStrLn "*** Solution to BinaryCode ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     readDigit d = ord d - ord '0'
>     go xs = let ns = map readDigit xs
>                 (d0s, d1s) = decode ns
>                 pstr Nothing = "NONE"
>                 pstr (Just nums) = concatMap show nums
>             in map pstr [d0s, d1s]

> examples = [ "123210122"
>            , "11"
>            , "22111"
>            , "123210120"
>            , "3"
>            , "12221112222221112221111111112221111"
>            ]
