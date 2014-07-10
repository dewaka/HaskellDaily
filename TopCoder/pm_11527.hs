-- http://community.topcoder.com/stat?c=problem_statement&pm=11527

{-
Problem Statement : DecodeDigits

A simple way to encode a word into a string of digits is to replace each
letter by its order in the alphabet. That is, "a" will change to "1", "b"
to "2", ..., and "z" to "26". For example, encode("cow")="31523"
and encode("cat")="3120".

Sadly, this encoding cannot always be uniquely decoded, because two
different words can yield the same string of digits when encoded. For
example, encode("beard")=encode("yard")="251184".

String A is a subsequence of string B if it is possible to erase some
letters of B (possibly none, possibly all of them) to obtain A. For
example, "cage" is a subsequence of "cabbages".

You are given a String D containing a string of digits. If there is no
string Y such that encode(Y)=D, return the String "NONE". Otherwise, find
and return the longest string X with the following property: whenever
encode(Y)=D, X is a subsequence of Y. If there are multiple such strings,
return the lexicographically smallest one.
-}

import Data.Char (ord, isLower, isAlpha)

encode :: Char -> Int
encode c
  | isAlpha c = 26 + ord c - ord (if isLower c then 'z' else 'Z')
  | otherwise = error $ "Cannot encode non-letter character: " ++ show c

encodeString :: String -> String
encodeString str = concat $ map (show . encode) str

main :: IO ()
main = do
  putStrLn "*** DecodeDigits Solution ***"
