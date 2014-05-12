-- http://projecteuler.net/problem=32
-- Pandigital products

-- n digit number is pandigital if it makes use of all the digits 1 to n exactly
-- once; for example, 15234 is 1 through 5 pandigital

import Data.Char (ord)

isPandigitalGeneric :: Int -> Int -> Int -> Bool
isPandigitalGeneric m n num = all check $ show num
  where
    check c = num >= m && num <= n
      where num = ord c - ord '0'

isPandigital :: Int -> Bool
isPandigital = isPandigitalGeneric 1 9



main =
  putStrLn "Problem 32"
