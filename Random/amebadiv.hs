-- http://community.topcoder.com/stat?c=problem_statement&pm=13088

growFrom x [] = x
growFrom x (y:ys) = if x == y then growFrom (2*x) ys
                    else growFrom x ys

-- possibleFinalState x gels = check x $ reverse gels
--   where
--     check 0 _ = False
--     check x [] = True
--     check x (y:ys) = if y*2 == x then check y ys
--                      else check x ys

testGels1 = [3, 2, 1]
testGels3 = [1, 2, 4, 8, 16, 32, 64, 128, 256, 1024, 2048]

                                   
