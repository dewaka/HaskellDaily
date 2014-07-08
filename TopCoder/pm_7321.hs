-- http://community.topcoder.com/stat?c=problem_statement&pm=7321

-- Problem Statement

-- Uncompress the given string s in the following manner: Find a section that
-- matches the pattern "k(q)" (quotes for clarity only), where k is a single digit,
-- the two parentheses are a matching set, and q is zero or more characters.

-- Replace the entire section with k consecutive occurrences of q.
-- Repeat this process until there are no more such patterns. Return the length of
-- the uncompressed string.


-- Definition

-- Class:	CompressedString
-- Method:	getLength
-- Parameters:	String
-- Returns:	int
-- Method signature:	int getLength(String s)
-- (be sure your method is public)


-- Constraints
-- -	s will contain between 0 and 50 characters, inclusive.
-- -	s will contain only digits ('0'-'9') and parentheses ('(',')').
-- -	The parentheses in s will be properly matched.
-- -	Each opening parenthesis ('(') in s will be preceded by a digit ('0'-'9').
-- -	The return value will be less than or equal to 2147483647.

import Data.List (foldl')

data CStr = Str String
          | NStr Int String deriving (Eq)

instance Show CStr where
  show (Str s) = s
  show (NStr n s) = concat $ take n $ repeat s

uncompress :: [CStr] -> String
uncompress = foldl' (\a c -> a ++ show c) []

-- TODO: Implement parsing function to read a String like 10342(76) and return
-- a parsed CStr list like: [CStr "1034", NStr 2 "76"]
parseCompressed :: String -> [CStr]
parseCompressed = undefined

main :: IO ()
main = do
  putStrLn "*** CompressedString Problem ***"
