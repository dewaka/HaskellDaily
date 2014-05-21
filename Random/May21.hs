-- http://www.reddit.com/r/dailyprogrammer/comments/263dp1/5212014_challenge_163_intermediate_fallouts/

import Data.Char
import Data.List

wordListFile = "enable1.txt"

-- nLengthWords n = filter lengthN
--   where
--     lengthN = flip (length . (==n))

nLengthWords = filter . (. length) . (==)

readWords :: String -> IO [String]
readWords file = do
  str <- readFile file
  return $ lines str

similarityIndex :: Eq a => [a] -> [a] -> Int
similarityIndex [] [] = 0
similarityIndex (x:xs) (y:ys)
  | x == y = 1 + similarityIndex xs ys
  | otherwise = similarityIndex xs ys
similarityIndex _ _ = error "Comparison lists should be same length"

mostSimilar :: Eq a => [a] -> [[a]] -> [([a], Int)]
mostSimilar w pool = sortBy (\(_, l1) (_, l2) -> l2 `compare` l1)
                     $ zip pool $ map (similarityIndex w) pool

main :: IO ()
main = do
  putStrLn "Starting game"
