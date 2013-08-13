import Data.List (tails)

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

-- TODO: Problem 5
-- What is the smallest number divisible by each of the numbers 1 to 20?
problem_5 = undefined


-- Problem 6
-- What is the difference between the sum of the squares and the square of the sums?

problem_6 = diff 10000
  where
    diff n = (sum [1..n])^2 - sum (map (^2) [1..n])

-- Problem 7
-- Find the 10001st prime.
problem_7 = primes !! 10001

-- Problem 8
-- Discover the largest product of five consecutive digits in the 1000-digit number.

largeNum = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

charToDigit n = read [n] :: Int

problem_8 = maximum $ map (product . take 5) (tails nums)
  where 
    nums = map charToDigit largeNum

