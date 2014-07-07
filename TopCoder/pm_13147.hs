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

main :: IO ()
main = do
  putStrLn "LongWordsDiv2 Problem"
