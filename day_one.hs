-- August 08, 2013
-- Even Odd Split
-- http://dailyhaskellexercise.tumblr.com/post/57051641046/even-odd-split

-- Write a function evenOddSplit :: [a] -> ([a], [a]) that takes a
-- list and split it in two, such that the values at even position is
-- in the first list and values in the odd position is in the second
-- list. 

evenOddSplit :: [a] -> ([a], [a])
evenOddSplit ls = go ls [] []
  where go [] es os = (es, os)
        go [x] es os = (es ++ [x], os)
        go (x:y:xs) es os = go xs (es++[x]) (os++[y])

-- This is a beautiful solution
evenOddSplit' [] = ([],[])
evenOddSplit' (x:xs) = (x:o, e)
  where (e,o) = evenOddSplit' xs

-- Same definition using foldr
evenOddSplit'' = foldr f ([], [])
  where f a (ls, rs) = (rs, a : ls) 

-------------------------------------------------------------------------------

-- Implements run-length encoding. runLengthEncoding :: (Eq
-- a)=>[a]->[(a,Int)]. It condenses a run of same elements to
-- a pair, an element and the number of repeats. 
-- http://dailyhaskellexercise.tumblr.com/post/57140686229/run-length-encoding
runLengthEncoding :: (Eq a) => [a] -> [(a, Int)]
runLengthEncoding [] = []
runLengthEncoding (x:xs) = (x,n) : runLengthEncoding rs
  where n = length xx + 1
        xx = takeWhile (==x) xs
        rs = dropWhile (==x) xs
