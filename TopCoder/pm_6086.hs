-- http://community.topcoder.com/stat?c=problem_statement&pm=6086

{-
Problem Statement: StringReplacements

You are given a String that initially contains only a single letter. After
every second of time, all occurrences of the letter 'a' are replaced
with "acb", all occurrences of the letter 'b' are replaced with "baa", and
all occurrences of the letter 'c' are replaced with "bcb". These replacements
happen simultaneously during each second. You are given three ints: left,
right, and nSeconds. Take the substring between positions left and
right (both 0-based), inclusive, after nSeconds, and return a int[]
containing exactly 3 elements. The first element is the number of 'a's
in the substring, the second element is the number of 'b's, and the
third element is the number of 'c's. For example, if letter = "a",
then after 2 seconds, the string will be "acbbcbbaa". If left = 2 and
right = 6, the substring is "bbcbb", which contains no 'a's, 4 'b's,
and 1 'c'. Therefore, you will return {0, 4, 1}.
-}

substitution [] = []
substitution ('a':xs) = "acb" ++ substitution xs
substitution ('b':xs) = "baa" ++ substitution xs
substitution ('c':xs) = "bcb" ++ substitution xs

select xs m n = take (n+1 - m) $ drop m xs

letterCounts xs = (count 'a', count 'b', count 'c')
  where
    count c = length $ filter (==c) xs

substitution' x = x' : substitution' x'
  where
    x' = substitution x

answer s m n t =
  let s' = substitution' s !! (t-1)
      sub = select s' m n
  in letterCounts sub

main :: IO ()
main = do
  putStrLn "*** StringReplacements Solution ***"
  print $ answer "a" 2 6 2
  print $ answer "a" 0 2 1
  print $ answer "c" 1 4 2
  print $ answer "b" 4 12 3
