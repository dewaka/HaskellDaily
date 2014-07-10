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

import Data.List (foldl', foldl1')
import Data.Char (ord, isLower, isAlpha, chr)
import Data.Maybe (isJust, fromJust)

encode :: Char -> Int
encode c
  | isAlpha c && isLower c = 26 + ord c - ord 'z'
  | otherwise = error $ "Cannot encode non-letter character: " ++ show c

encodeString :: String -> String
encodeString str = concatMap (show . encode) str

properSubSeq :: [a] -> [[a]]
properSubSeq [] = []
properSubSeq (x:xs) = xs : [x:s | s <- properSubSeq xs] ++ properSubSeq xs

subSeq :: [a] -> [[a]]
subSeq xs = xs : properSubSeq xs

decodeDigit :: Int -> Maybe Char
decodeDigit n
  | 1 <= n && n <= 26 = Just $ chr $ ord 'a' + n-1
  | otherwise = Nothing

-- This is a function which can build a stream of extractions from a list
-- with following properties
-- "abc" -> ["a", "b", "c"], ["ab", "c"], ["a", "bc"]
-- In other words this function should extract elements to sub lists in terms of
-- groups of ones and two (consecutive items)
extract :: [a] -> [[[a]]]
extract [] = []
extract [x] = [[[x]]]
extract [x,y] = [[[x], [y]], [[x, y]]]
extract (x:y:xs) = withOnes ++ withTwos
  where
    withOnes = map ([[x]] ++) $ extract (y:xs)
    withTwos = map ([[x, y]] ++) $ extract xs

decodeString :: [String] -> Maybe String
decodeString xs = go xs []
  where
    go [] acc = Just $ reverse acc
    go (x:xs) acc =
      case decodeDigit $ read x of
        Nothing -> Nothing
        Just c -> go xs (c:acc)

possibleSuggestions :: String -> [String]
possibleSuggestions s =
  let exs = extract s
      dexs = map decodeString exs
  in map fromJust $ filter isJust dexs

intersection xs ys = foldl' f [] xs
  where
    f acc x = if x `elem` ys then x:acc else acc

testExamples = [ ("38956", "chief")
               , ("13919156", "if")
               , ("1122", "")
               , ("3120", "cat")
               , ("0", "NONE")
               ]

answer s =
  case possibleSuggestions s of
    [] -> Nothing
    sugs -> Just $ foldl1' intersection sugs

-- NOTE: The 2nd case the answer generated does not agree with the given one!
main :: IO ()
main = do
  putStrLn "*** DecodeDigits Solution ***"
  mapM_ print $ map (answer . fst) testExamples
