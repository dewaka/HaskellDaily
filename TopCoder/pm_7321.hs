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
import Data.Char (ord)

data CStr = Str String
          | NStr Int [CStr] deriving (Eq)

instance Show CStr where
  show (Str s) = s
  show (NStr n s) = concat $ take n $ repeat (show s)

uncompress :: [CStr] -> String
-- uncompress = foldl' (\a c -> a ++ show c) []
uncompress [] = []
uncompress (Str s:xs) = s ++ uncompress xs
uncompress ((NStr n sub):xs) =
  let fs = foldl' (\a c -> a ++ uncompress [c]) [] sub
  in (concat $ take n $ repeat fs) ++ uncompress xs

uncompressedLength = length . uncompress

splitUntil :: (a -> Bool) -> [a] -> ([a], [a])
splitUntil _ [] = ([], [])
splitUntil p y@(x:xs)
  | p x = ([], y)
  | otherwise = let (fs, rs) = splitUntil p xs
                in (x:fs, rs)

-- TODO: Implement parsing function to read a String like 10342(76) and return
-- a parsed CStr list like: [CStr "1034", NStr 2 "76"]
parseCompressed :: String -> [CStr]
parseCompressed str = go str []
  where
    go [] acc = [Str acc]
    go (x:'(':xs) acc = let (fs, ps) = extractFirstParenthesis ('(':xs)
                            n = ord x - ord '0'
                        in [Str acc, NStr n $ parseCompressed fs] ++ parseCompressed ps
    go (x:xs) acc = go xs (acc++[x])

extractFirstParenthesis :: String -> (String, String)
extractFirstParenthesis str = go str []
  where
    go ('(':xs) [] = go xs ['(']
    go ('(':xs) ps = let (ys,rs) = go xs ('(':ps)
                     in  ('(':ys, rs)
    go (')':xs) [] = error "Unmatched parenthesis"
    go (')':xs) ['('] = ([], xs) -- Only one closing brace remaining. So this is the end of the first match. Return the string and the rest of non-consumed string
    go (')':xs) (_:ps) = let (ys, rs) = go xs ps
                         in (')':ys, rs)
    go (x:xs) ps = let (ys, rs) = go xs ps
                   in (x:ys, rs)
    go [] [] = ([], [])

testStrings = [ ("123", 3)
              , ("10342(76)", 8)
              , ("33(562(71(9)))", 19)
              , ("0(0)", 0)
              , ("1(1(1(1(1(1(1(0(1234567890))))))))", 0)
              ]

answer = mapM_ print $ map (uncompressedLength . parseCompressed . fst) testStrings

main :: IO ()
main = do
  putStrLn "*** CompressedString Problem ***"
  answer
