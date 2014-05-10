-- http://www.reddit.com/r/haskell/comments/254txi/need_a_program_to_count_word_occurrences_in_a/

-- Program to count unique words in a string

import Data.List
import qualified Data.Map as M

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p ls = go p ls []
	where
		go _ [] acc      = [acc]
		go p (x:xs) acc = if p x then acc : go p xs []
						  else go p xs (acc++[x])

splitByComma :: String -> [String]
splitByComma = splitBy (==',')

updateWordCount :: M.Map String Int -> String -> M.Map String Int
updateWordCount = flip $ M.alter incrementCount
  where
    incrementCount Nothing = Just 1
    incrementCount (Just n) = Just (n+1)

trim :: String -> String
trim = reverse . trimFront . reverse . trimFront
  where
    trimFront = dropWhile (==' ')

exampleOne =
  let ws = words "apple banana mango apple banana grapefruit orange apple"
      fq = foldl updateWordCount M.empty ws
  in print fq

wordFrequencyMap = foldl updateWordCount M.empty

main :: IO ()
main = do
	putStrLn "Working on the problem"
	print $ splitByComma "Hello, there"
