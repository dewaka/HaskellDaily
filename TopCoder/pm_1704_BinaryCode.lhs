http://community.topcoder.com/stat?c=problem_statement&pm=1704

Let's say you have a binary string such as the following:

011100011

One way to encrypt this string is to add to each digit the sum of its adjacent
digits. For example, the above string would become:

123210122

In particular, if P is the original string, and Q is the encrypted string,
then Q[i] = P[i-1] + P[i] + P[i+1] for all digit positions i. Characters off
the left and right edges of the string are treated as zeroes.

> encode nums = go (0:nums)
>   where
>     go (x:xs@(y:z:_)) = x+y+z : go xs
>     go [x,y] = [x+y]
>     go _ = []

> main :: IO ()
> main = do
>   putStrLn "*** Solution to BinaryCode ***"
>   answer

> answer = undefined
