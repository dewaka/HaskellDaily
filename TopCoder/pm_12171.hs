-- http://community.topcoder.com/stat?c=problem_statement&pm=12171

{-
 Problem Statement : IndependentOfOR

Let S be a set containing k non-negative integers: { S[0], ..., S[k-1] }.
We define OR(S) to be the bitwise "or" of all elements of S.
Formally: OR(S) = S[0] or S[1] or ... or S[k-1]. For consistency, if S is the
empty set, then OR(S) is defined to be 0.

A set T is called "independent with respect to OR" if no two subsets of T
produce the same value when OR is applied to them. Formally, T should have the
following property: | { OR(S) : S is a subset of T } | = 2^|T|. In words: the
set of all values OR(S), where S is a subset of T, contains exactly
(2 to the size of T) distinct values.

You are given a int[] A that describes a set of integers. Your goal is to
select a subset B of the set described by A. The subset B has to be independent
with respect to OR, and the sum of its elements has to be as large as possible.
Return the largest possible sum of elements of the set B.
-}

import Data.Bits ((.|.))
import Data.List (foldl1', nub)

bitOrOfSet [] = 0
bitOrOfSet xs = foldl1' (.|.) xs

properSubSets :: [a] -> [[a]]
properSubSets [] = []
properSubSets (x:xs) = xs : [x:s | s <- properSubSets xs] ++ properSubSets xs

subSets :: [a] -> [[a]]
subSets xs = xs : properSubSets xs

independantWrtOr xs =
  let sors = map bitOrOfSet $ subSets xs
      uniqSors = nub sors
  in (length sors == length uniqSors)

independantWrtOr' xs = bitOrOfSet xs == 2 ^ length xs


main :: IO ()
main = do
  putStrLn "*** IndependentOfOR Solution ***"
