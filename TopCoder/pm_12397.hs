-- http://community.topcoder.com/stat?c=problem_statement&pm=12397

{-
Problem Statement: TheFrog

Frog Jim is fond of jumping. Moreover, all his jumps have the same length X.
Currently, he is standing at the point 0 on the real axis. He needs to jump
along the axis until he gets to point D or beyond it.

There are several pits between points 0 and D. For each valid i, there is a pit
strictly between L[i] and R[i]. This means that Jim cannot step onto any point
that is strictly between points L[i] and R[i]. (He is allowed to step onto the
points L[i] and R[i]: as all pits are disjoint, the endpoints of each pit are
always safe.)

Find the minimum length of jump X such that if Jim performs only jumps of
length X, he can avoid every pit and get to point D or any point beyond it.
Note that this length can be non-integer.
-}

{-
0)

16
{2}
{7}
Returns: 7.0
Moving by jumps of length 7, Jim goes from point 0 to point 7, then to point 14 and then to point 21, which is beyond 16. If Jim chose a shorter jump, he would end up in the pit.
1)

25
{11, 3}
{21, 7}
Returns: 10.5
There are two pits. One of them is between points 11 and 21 and the other between points 3 and 7. By fixing jump length at 10.5, Jim successfully avoids them.
-}

minJump t (m, n)
  | t <= m = t
  | t >= n = if (n-m) >= m
             then n
             else n/2
  | otherwise = error $ "Cannot make jump"

main :: IO ()
main = do
  putStrLn "*** TheFrog Problem ***"
