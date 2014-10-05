-- Write a number as sum of squares
-- Problem found in 'ML for the working programmer' page 93

numAsSquareSums :: Int -> [(Int, Int)]
numAsSquareSums num = between x1 y1
  where
    x1 = floor $ sqrt $ fromIntegral num
    y1 = 0
    between x y
      | x < y = []
      | x^2 + y^2 < num = between x (y+1)
      | x^2 + y^2 == num = (x,y) : between (x-1) (y+1)
      | x^2 + y^2 > num && num > x^2 + (y-1)^2 = between (x-1) y
