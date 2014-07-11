-- http://community.topcoder.com/stat?c=problem_statement&pm=10329

countNum n num
  | 0 < n && n <= 9 = go num n 0
  | n == 0 = go num n 0 - 1
  | otherwise = error $ "Invalid number to count occarances: " ++ (show n)
  where
    go num n acc
      | num > n = if (num - n) `rem` 10 == 0 then go (num `div` 10) n (acc+1)
                  else go (num `div` 10) n acc
      | num == n = acc+1
      | otherwise = acc

countNumInRange n = sum . map (countNum n)

answer :: Int -> [(Int, Int)]
answer p = zipWith (\i _ -> (i, countNumInRange i [1..p])) [0..9] [0..9]

testExamples = [ 7, 11, 19, 999, 543212345 ]

-- Todo: Optimize the solution
-- When computing the last one runs into out of memory errors!
main :: IO ()
main = do
  putStrLn "*** PageNumbers Solution ***"
  mapM_ (print . answer) testExamples
