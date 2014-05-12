import Data.Char (ord)

-- http://projecteuler.net/problem=30

bound = 6*(9^5)

numDigits num = map (\c -> ord c - ord '0') $ show num

searchPow5Nums b = filter test [2..b]
  where
    test num = num == sum (map (^5) $ numDigits num)

-- http://projecteuler.net/problem=31

coins = [1,2,5,10,20,50,100]

-- how many ways 200 can be given as combinations of above coins
