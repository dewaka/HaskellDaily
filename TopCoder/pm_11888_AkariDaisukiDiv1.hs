-- http://community.topcoder.com/stat?c=problem_statement&pm=11888

{-
Problem Statement : AkariDaisukiDiv1

Consider the following function:

f(X) = Waai + X + Akari + X + Daisuki

Here, X is a string and + denotes string concatenation. Waai, Akari and
Daisuki are constant non-empty strings.

You are given 5 Strings Waai, Akari, Daisuki, S and F, and a int k.

Compute how many times F occurs in f^k(S) as its substring.
The notation f^k(S) means that f is applied to S exactly k times in a row.
Since the number can be quite large, compute the number modulo 1,000,000,007.
-}

akariDaisukFunc w a d x = w ++ x ++ a ++ x ++ d

isPrefix [] _          = True
isPrefix _ []          = False
isPrefix (x:xs) (y:ys) = (x==y) && isPrefix xs ys

countSubstring [] _ = 0
countSubstring _ [] = 0
countSubstring haystack needle = go haystack 0
  where
    go [] acc = acc
    -- We have to find overlapping substrings as well.
    -- That is why we cannot skip (i.e. drop (length needle) s) on
    -- later iterations
    go s@(_:xs) acc = go xs (if isPrefix needle s then acc + 1 else acc)

-- This is the same as: iterations f x !! n
-- This function did not improve performance as far as calculating large tests
-- were concerned
applyNTimes _ x 0 = x
applyNTimes f x n = applyNTimes f (f x) (n-1)

answer waai akari daisuki s f k =
  -- let str2 = iterate (akariDaisukFunc waai akari daisuki) s !! k
  let str = applyNTimes  (akariDaisukFunc waai akari daisuki) s k
  in countSubstring str f

testData = [ (("a", "b", "c", "x", "axb", 2), 2)
           , (("a", "b", "c", "x", "abcdefghij", 1), 0)
           , (("a", "a", "a", "b", "aba", 2), 4)
          -- , (("a", "b", "c", "d", "baadbdcbadbdccccbaaaadbdcbadbdccbaadbdcba", 58), 191690599)
           ]

main :: IO ()
main = do
  putStrLn "*** AkriDaisukiDiv1 Solution ***"
  mapM_ printAns testData
  where
    printAns (args, e) = do
      let (waai, akari, daisuki, s, f, k) = args
      let ans = answer waai akari daisuki s f k
      putStrLn $ "Answer: " ++ (show ans) ++ ", Expected: " ++ (show e)
