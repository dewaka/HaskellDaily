-- http://www.reddit.com/r/dailyprogrammer/comments/29od55/722014_challenge_169_intermediate_homerow_spell/

firstRow = "qwertyuiop"
secondRow = "asdfghjkl"
thirdRow = "zxcvbnm"

dictFile = "brit-a-z.txt"

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
  contents <- readFile file
  return $ map (filter (/= '\r')) $ lines contents

testValidWords :: FilePath -> String -> IO ()
testValidWords file word = do
  dict <- readDictionary file
  let ps = possibleShiftedWords word
  print $ length dict
