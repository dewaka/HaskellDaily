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
primes = 2 : filter ((==1). length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
      | p*p > n = [n]
      | n `rem` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise = factor n ps

problem_3 = last $ primeFactors 317584931803

-- Problem 4
-- Find the largest palindrome made from the product of two 3-digit numbers.

                
problem_4 = maximum [x | m <- [100..999], n <- [m..999], let x = m*n, let s = show x, isPalindrome s]
  where 
    isPalindrome s = s == reverse s
