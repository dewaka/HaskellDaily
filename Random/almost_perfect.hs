-- http://community.topcoder.com/stat?c=problem_statement&pm=7557

-- Problem Statement
-- An integer x is called a proper divisor of an integer y
-- if x is a divisor of y and 1 <= x < y.
-- Let us denote as s(a) the sum of all proper divisors of a.
-- An integer a is called almost perfect by k if |a-s(a)| <= k.
-- You are given ints left, right and k.
-- Return the number of integers between left and right, inclusive,
-- that are almost perfect by k.

divisor x y = y `rem` x == 0

properDivisor x y = 1 <= x && x < y && divisor x y

sumProperDivisors x = sum [n | n <- [1..x-1], properDivisor n x]

almostPerfect k n = abs (n - sumProperDivisors n) <= k

numberOfAlmostPerfectNumbers left right k = length $ filter (almostPerfect k) [left..right]

main :: IO ()
main = do
  putStrLn "Hello there"
