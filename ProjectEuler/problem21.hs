-- Amicable pairs problem
-- http://projecteuler.net/problem=21

properDivisors n = [m | m <- [1..n `div` 2], n `rem` m == 0]
sumDivisors = sum . properDivisors

amicablePair a b = a /= b && sumDivisors a == b && sumDivisors b == a

-- This is not very efficient. computes the same pairs twice
amicablePairs b = [(m, n) | m <- [1..b], let n = sumDivisors m, amicablePair m n]

sumOfAmicablePairs b = sum (map fst $ amicablePairs b)

answer21 = sumOfAmicablePairs 10000

main = print answer21