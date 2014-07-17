ttp://community.topcoder.com/stat?c=problem_statement&pm=12994

Problem Statement: PackingBallsDiv1

We have balls of K different colors. The colors are numbered 0 through K-1, and
the number of balls of color i is X[i]. We want to divide the balls into as few
packages as possible. Each package must contain between 1 and K balls, inclusive.
Additionally, each package must be either a "normal set" (all balls in the
package have the same color), or a "variety set" (no two balls have the same color).

You are given the int K. You are also given ints A, B, C, and D. Use these to
generate the array X using the following pseudocode:

X[0] = A
for i = 1 to K-1:
    X[i] = (X[i-1] * B + C) % D + 1

Compute and return the smallest possible number of packages.

> generateBalls k a b c d
>   | k < 1 = error "Invalid k"
>   | otherwise = generate k [a]
>   where
>     generate 1 acc = acc
>     generate k acc@(x:_) = generate (k-1) (n:acc)
>       where
>         n = (x * b + c) `rem` d + 1

> main :: IO ()
> main = do
>   putStrLn "*** PackingBallsDiv1 Solution ***"
