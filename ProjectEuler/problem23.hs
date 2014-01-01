properDivisors n = [m | m <- [1..n `div` 2], n `rem` m == 0]
sumDivisors = sum . properDivisors

-- http://projecteuler.net/problem=23
perfectNumbers b = [n | n <- [1..b], n == sumDivisors n]
deficientNumbers b = [n | n <- [1..b], n > sumDivisors n]
abundantNumbers b = [n | n <- [1..b], n < sumDivisors n]

abundantNumLimit = 28123        -- numbers greater than this can be written as sum of two abundant numbers

numAsAbundantSum num =
  let anSeries = abundantNumbers num -- abundant numbers less than num
      find p [] = Nothing
      find p s@(x:xs) = if p+x > num
                        then Nothing
                        else case dropWhile (\n -> n + p < num) s of
                          [] -> Nothing
                          (y:_) -> if p+y==num
                                   then Just (p,y)
                                   else Nothing
  in case anSeries of
    [] -> Nothing
    (x:xs) -> find x xs

numbersNonAbundant b = let rs = filter cond $ zip [1..b] (map numAsAbundantSum [1..b])
                       in map fst rs
  where cond (_, Nothing) = True
        cond _ = False

answer21 = sum $ numbersNonAbundant abundantNumLimit
