-- http://community.topcoder.com/stat?c=problem_statement&pm=2911&rd=5849

import Data.List (sortBy)

matchIndex xs ys = go xs ys 0
  where
    go (x:xs) (y:ys) n = go xs ys (if x==y then n+1 else n)
    go _ _ n           = n

type Name = String
type Answers = String

sortAnswersByName :: [(Name, Answers)] -> [(Name, Answers)]
sortAnswersByName = sortBy (\(n,_) (m,_) -> n `compare` m)

sortByMatchAndName xs = sortBy compMatchName xs
  where
    compMatchName (n1, a1) (n2, a2) =
      case a2 `compare` a1 of
        EQ -> n1 `compare` n2
        e -> e

matchFirst (w:ws) ms =
  let (name, ans) = w
      midx = map (\(n, a) -> (n, matchIndex ans a)) ms
      (m:_) = sortByMatchAndName midx
  in fst m

findMatch :: Name -> [(Name, Answers)] -> [(Name, Answers)] -> Maybe Name
findMatch name ws ms = go name (sortAnswersByName ws) ms
  where
    go name ws@(w:ws') ms
      | name == fst w = Just $ matchFirst ws ms
      | otherwise = let m = matchFirst ws ms
                        rms = filter (\(n,_) -> n /= m) ms
                    in go name ws' rms
    go _ _ _ = Nothing

testData = [((["Constance", "Bertha", "Alice"], ["aaba", "baab", "aaaa"])
           , (["Chip", "Biff", "Abe"], ["bbaa", "baaa", "aaab"])
           , ("Bertha", "Biff"))
           , ((["Constance", "Bertha", "Alice"], ["aaba", "baab", "aaaa"])
           , (["Chip", "Biff", "Abe"], ["bbaa", "baaa", "aaab"])
           , ("Constance", "Chip"))
           , ((["Constance", "Alice", "Bertha", "Delilah", "Emily"], ["baabaa", "ababab", "aaabbb", "bababa", "baabba"])
           , (["Ed", "Duff", "Chip", "Abe", "Biff"], ["aabaab", "babbab", "bbbaaa", "abbbba", "abaaba"])
           , ("Constance", "Duff"))
           ]

answer ((w, wa), (m, ma), (wname, mname)) =
  (findMatch wname (zip w wa) (zip m ma), mname)

main :: IO ()
main = do
  putStrLn "*** MatchMaking Solution ***"
  mapM_ (print . answer) testData
