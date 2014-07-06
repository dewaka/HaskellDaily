-- http://community.topcoder.com/stat?c=problem_statement&pm=7671

-- A String p is called anti-palindrome if p[i] doesn't equal to p[n - i - 1]
-- for each 0 <= i < (n-1)/2, where n is the length of p. It means that each
-- character (except the middle in the case of a string of odd length) must be
-- different from its symmetric character. For example, "c", "cpp", "java" are
-- anti-palindrome, but "test", "pp" and "weather" are not.

-- You are given a String s. Rearrange its letters in such a way that the
-- resulting string is anti-palindrome. If there are several solutions, return
-- the one that comes earliest alphabetically. If it is impossible to do it,
-- return the empty string.

import Data.List (sort, permutations)

-- TODO: This is a very inefficient solution. Improve!!!

antiPalindrome xs = check xs (reverse xs) (length xs `div` 2)
  where
    check (x:xs) (y:ys) n = n==0 || (x/=y && check xs ys (n-1))
    check _ _ _ = True

antiPalindromes str =
  case sort $ filter antiPalindrome $ permutations str of
    [] -> Nothing
    (first:_) -> Just first

main :: IO ()
main = do
  putStrLn "Anti-palindromes"
