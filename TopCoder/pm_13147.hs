-- http://community.topcoder.com/stat?c=problem_statement&pm=13147

--  Problem: LongWordsDiv2

{-
Problem Statement

Fox Ciel likes all the words that have the following properties:
Each letter of the word is an uppercase English letter.
Equal letters are never consecutive.
There is no subsequence of the form xyxy, where x and y are (not necessarily
distinct) letters. Note that a subsequence doesn't have to be contiguous.

Examples:
Ciel does not like "ABBA" because there are two consecutive 'B's.
Ciel does not like "THETOPCODER" because it contains the subsequence "TETE".
Ciel does not like "ABACADA" because it contains the subsequence "AAAA".
   (Note that here x=y='A'.)
Ciel likes "A", "ABA", and also "ABCBA".

Given a String word, return "Likes" (quotes for clarity) if Ciel likes
  word and "Dislikes" if she does not.
-}

import Data.Char (isLower)

canFindSubsequence [] _ = True
canFindSubsequence [] [] = True
canFindSubsequence _ [] = False
canFindSubsequence sub@(x:xs) hay@(y:ys)
  | x == y = canFindSubsequence xs ys
  | otherwise = canFindSubsequence sub $ dropWhile (/=x) hay

-- strCombinations "abc"
-- [('a',"bc"),('b',"c"),('c',"")]
singleCombinations [] = []
singleCombinations (x:xs) = (x, xs) : singleCombinations xs

twoLetterCombinations [] = []
twoLetterCombinations (x:xs) =
  let ys = map (\(c, cs) -> ([x,c], cs)) $ singleCombinations xs
  in ys ++ twoLetterCombinations xs

checkCombinationExist str =
  let combinations = twoLetterCombinations str
  in any id $ map (uncurry canFindSubsequence) combinations

hasConsecutiveLetters (x:y:xs) = x == y || hasConsecutiveLetters (y:xs)
hasConsecutiveLetters _ = False

likesString str = allUpper && noConsecutiveCombinations && noSubSequences
  where
    allUpper = not $ any isLower str
    noConsecutiveCombinations = not $ hasConsecutiveLetters str
    noSubSequences = not $ checkCombinationExist str

testStrings = [("AAA", False),
               ("ABCBA", True),
               ("ABCBAC", False),
               ("TOPCODER", True),
               ("VAMOSGIMNASIA", False),
               ("SINGLEROUNDMATCH", True),
               ("DALELOBO", True)]

testRun =
  let res = map (likesString . fst) testStrings
  in mapM_ print $ zipWith (\(str, _) b -> (str, b)) testStrings res

main :: IO ()
main = do
  putStrLn "*** LongWordsDiv2 Problem ***"
  testRun
