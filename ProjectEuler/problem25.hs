-- http://projecteuler.net/problem=25
-- 1000 - digit Fibonacci number
fib n = tfib 1 1 n
  where
    tfib a b n
      | n <= 1 = a
      | otherwise = tfib b (a+b) (n-1)

-- Do not actually try to compute this answer ;)
-- My machine is too slow for that
answer25 = let numLen = length . show
               million = 10^6
               (ans:_) = dropWhile (\n -> numLen n < million) $ map fib [1..]
           in ans
