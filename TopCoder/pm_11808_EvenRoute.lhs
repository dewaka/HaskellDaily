http://community.topcoder.com/stat?c=problem_statement&pm=11808

Problem Statement: EvenRoute

Fox Ciel has stumbled upon a new problem: In this problem you will visit some
points with integer coordinates in the Cartesian plane. Initially, you are
located at the point (0,0). In each step, you can move from your current point
to one of the four directly adjacent points. I.e., if you are at (x,y), you can
move to one of the points (x,y+1), (x,y-1), (x+1,y), and (x-1,y).

You are given two int[]s x and y, each containing N elements. Together, x and y
describe N distinct points in the Cartesian plane: for 0 <= i < N, point i has
the coordinates (x[i],y[i]).

The goal is to find a sequence of steps that satisfies the following criteria:
The starting point is (0,0).
The sequence visits each of the N given points at least once.
The sequence finishes in one of the given points.
Mr. K claims to have solved this problem but Ciel does not believe him. Ciel has
prepared a method to verify Mr. K's claims. Given an int wantedParity, the
parity of the number of steps in the sequence found by Mr. K, Ciel will find if
it is possible to find a sequence that follows the previously mentioned
conditions and a new one:

The parity of the total number of steps is wantedParity. In other words, if
wantedParity=0 then the total number of steps must be even, and if
wantedParity=1 then the total number of steps must be odd.
Return "CAN" (quotes for clarity) if at least one such sequence of steps exists,
and "CANNOT" otherwise.

Given position (x, y) possible next positions are.

> next (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

We need a function to check whether one can reach

> nextN n start
>   | n > 1 = concat $ map (nextN (n-1)) $ next start
>   | n == 1 = next start
>   | otherwise = [start]

> canReachIn steps start end = end `elem` ns
>   where
>     ns = nextN steps start

I think the following function is extremely inefficient mainly becuase the way
canReachin works is very inefficient.

> countMinStepsTo start end = go 0
>   where
>     go n = if canReachIn n start end then n
>            else go (n+1)

> countMinStepsTo' (x1, y1) (x2, y2) = s1 + s2
>   where
>     s1 = abs $ x2 - x1
>     s2 = abs $ y2 - y1

> countCumulativeSteps start steps = go (start:steps)
>   where
>     go (x:y:xs) = countMinStepsTo' x y + go (y:xs)
>     go _ = 0

Notes:
- The actual thing we are concerned about is whether we can reach all the points
  with an odd or even number of steps.
- The tricky point is that we have to try different permutations of points if we
  find that the given order of points cannot be reached with the required parity.
- Only after trying all the permutations that we can definitely say No. (If we
  get a Yes early on we have no need to try differnt permutations)

> selections xs = go xs []
>   where
>     go [] acc = []
>     go (x:xs) acc = (x, acc++xs) : go xs (acc++[x])

Following is a much more elegant version of the selections function based on
http://www.haskell.org/pipermail/haskell-cafe/2002-June/003122.html

> selections' [] = []
> selections' (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- selections' xs]

> permutations [] = [[]]
> permutations xs =
>   [ y:zs
>   | (y, ys) <- selections xs
>   , zs <- permutations ys]

Now we have a function to generate permutations of the points we have to traverse.
We got to find whether at least one of them would satisfy our condition.

> checkSteps p ps = any p $ map (countCumulativeSteps (0, 0)) $ permutations ps

> examples = [ (0, [-1,-1,1,1], [-1,1,1,-1])
>            , (1, [-5,-3,2], [2,0,3])
>            , (1, [1001, -4000], [0,0])
>            , (0, [11, 21, 0], [-20, 42, 7])
>            , (1, [0, 6], [10, -20])
>            ]

> answer = mapM_ (print . go) examples
>   where
>     go (n, xs, ys) =
>       let zs = zip xs ys
>           p = if n == 0 then even else odd
>       in if checkSteps p zs then "CAN"
>          else "CANNOT"

> main :: IO ()
> main = do
>   putStrLn "*** Solution to EvenRoute ***"
>   answer
