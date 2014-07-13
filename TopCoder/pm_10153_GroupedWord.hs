-- http://community.topcoder.com/stat?c=problem_statement&pm=10153

import Data.List (permutations)

isGrouped [] = True
isGrouped (x:xs) = go xs x [x]
  where
    go [] _ _ = True
    go (y:ys) x acc = not (y/=x && y `elem` acc) && go ys y (y:acc)

canBeGrouped xs = or [isGrouped $ concat ps | ps <- permutations xs]

findGroup xs =
  let gs = filter (isGrouped . concat) $ permutations xs
  in case gs of
    [] -> Nothing
    cs@(c:_) -> Just (concat c, length cs)

answer xs =
  case findGroup xs of
    Nothing -> "IMPOSSIBLE"
    Just (s, n) -> if n > 0
                   then "MANY"
                   else s

main :: IO ()
main = do
  putStrLn "*** GroupedWord Solution ***"
  print $ answer ["te", "st"]
  print $ answer ["te", "s", "t"]
