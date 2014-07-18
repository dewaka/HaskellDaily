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

Is there a recursive solution to this problem. Let's consider some specific cases.
Let's say k = 2 and X = [1, 1]. Then clearly the solution is just 1
(have just one bag with each of those mixed color balls).

k = 2, X = [1,2] -> Then optimal solution is 2 bags.
Could be like [A, A], [B] or [A, B], [B]

k = 2, X = [1,2,3] -> Optimal solution 3
[A, B, C], [A, A], [A, B]. This is a tricky one... This is the only best arrangement
possible. So any algorithmic solution should take care of solutions like this.

One brute-force possiblity is to find all legitimate solutions and select the ones
with minimum amount of bags. This is logically sound but this could mean exploring
expoential solution spaces.
Even if it can be solved that way, how to find the legitimate solutions. That is
the next logical question.
May be answering that question will lead to ways to find optimal solutions more
efficently as well.

k = 2, X = [2, 2, 3]
The best we can do is 4 bags. We cannot do it in less than that amount.

> main :: IO ()
> main = do
>   putStrLn "*** PackingBallsDiv1 Solution ***"
