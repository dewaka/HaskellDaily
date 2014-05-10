-- http://www.reddit.com/r/haskell/comments/254txi/need_a_program_to_count_word_occurrences_in_a/

-- Program to count unique words in a string

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p ls = go p ls []
	where
		go _ [] acc      = [acc]
		go p (x:xs) acc = if p x then acc : go p xs []
						  else go p xs (acc++[x])

splitByComma :: String -> [String]
splitByComma = splitBy (==',')

main :: IO ()
main = do
	putStrLn "Working on the problem"
	print $ splitByComma "Hello, there"