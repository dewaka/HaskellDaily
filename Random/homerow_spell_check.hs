-- http://www.reddit.com/r/dailyprogrammer/comments/29od55/722014_challenge_169_intermediate_homerow_spell/

import System.IO

firstRow = "qwertyuiop"
secondRow = "asdfghjkl"
thirdRow = "zxcvbnm"

dictFile = "Random/enable1.txt"

shiftFrom :: String -> Char -> Int -> Maybe Char
shiftFrom row c n =
  case lookup c $ zip row [0..] of
    Nothing -> Nothing
    Just m -> let n' = (m + n) `rem` length row
              in if n' >= 0 then Just (row !! n')
                 else Just (row !! (length row + n'))

shift :: Char -> Int -> Char
shift c n =
  let ps = [shiftFrom row c n | row <- [firstRow, secondRow, thirdRow]]
      [(Just c')] = filter (/= Nothing) ps
  in c'

shiftRight = shift
shiftLeft c n = shift c (-n)

shiftWord word n = [shift c n | c <- word]
shiftWordRight = shiftWord
shiftWordLeft word n = shiftWord word (-n)

possibleShiftedWords word = [shiftWord word n | n <- [-2..2]]

type Dict = [String]

hasWord :: Dict -> String -> Bool
hasWord dict word = word `elem` dict

validWords dict words = [w | w <- words, hasWord dict w]

readDictionary :: FilePath -> IO Dict
readDictionary file = do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8_bom
  contents <- hGetContents handle
  return $ map (filter (/= '\r')) $ lines contents

testValidWords :: FilePath -> String -> IO ()
testValidWords file word = do
  dict <- readDictionary file
  let ps = possibleShiftedWords word
  print $ filter (hasWord dict) ps

type Suggestion = (String, [String])

printSuggestions :: Suggestion -> IO ()
printSuggestions (word, suggestions) = do
  putStr $ word ++ " -> "
  print suggestions

getSuggestions :: Dict -> String -> Suggestion
getSuggestions dict word = (word, suggestions)
  where
    suggestions = filter (hasWord dict) $ possibleShiftedWords word

printCorrectedSentence :: String -> IO ()
printCorrectedSentence str = do
  dict <- readDictionary dictFile
  mapM_ (printWordOrSuggestions dict) $ words str
  where
    printWordOrSuggestions dict w =
      if hasWord dict w
      then putStrLn w
      else printSuggestions $ getSuggestions dict w

testSentence1 = "the quick ntpem fox jumped over rgw lazy dog"
testSentence2 = "gwkki we are hyptzgsi martians rt zubq in qrsvr"
testSentence3 = "a oweaib who fprd not zfqzh challenges should mt ewlst to kze"
