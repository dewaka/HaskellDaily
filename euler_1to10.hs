-- Problem 1
-- Add all the natural numbers below 1000 that are multiples of 3 or 5

problem_1 = sum $ filter divides3Or5 [1..1000]
    where divides3Or5 n = n `rem` 3 == 0 || n `rem` 5 == 0

-- Problem 2
-- Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed one million.
problem_2 = sum $ takeWhile (<= 1000000) $ filter even fibs
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Problem 3 
-- Find the largest prime factor of 317584931803. 
