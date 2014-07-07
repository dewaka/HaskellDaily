-- http://community.topcoder.com/stat?c=problem_statement&pm=13088

import Data.List (nub)

growFrom x [] = x
growFrom x (y:ys) = if x == y then growFrom (2*x) ys
                    else growFrom x ys

possibleFinalState state gels = check state $ reverse gels
  where
    check _ [] = True
    check x (y:ys)
      | x == y = False
      | x == 2*y = check y ys
      | otherwise = check x ys

impossibleInitialStates candidates states =
  nub $ filter (\x -> not $ possibleFinalState x states) candidates

numImpossibleInitialStates states =
  length $ impossibleInitialStates states states

testGels1 = [3, 2, 1]
testGels2 = [2,2,2,2,2,2,4,2,2,2]
testGels3 = [1, 2, 4, 8, 16, 32, 64, 128, 256, 1024, 2048]
testGels4 = [854,250,934,1000,281,250,281,467,854,562,934,1000,854,500,562]
