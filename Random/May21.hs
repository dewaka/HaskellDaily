-- http://www.reddit.com/r/dailyprogrammer/comments/263dp1/5212014_challenge_163_intermediate_fallouts/

import Data.Char

wordListFile = "enable1.txt"

readWords :: String -> IO [String]
readWords file = do
  str <- readFile file
  return $ lines str

main :: IO ()
main = do
  putStrLn "Starting game"
