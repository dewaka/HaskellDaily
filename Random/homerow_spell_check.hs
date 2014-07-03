-- http://www.reddit.com/r/dailyprogrammer/comments/29od55/722014_challenge_169_intermediate_homerow_spell/

firstRow = "qwertyuiop"
secondRow = "asdfghjkl"
thirdRow = "zxcvbnm"

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
