-- http://www.reddit.com/r/dailyprogrammer/comments/263dp1/5212014_challenge_163_intermediate_fallouts/

import Data.Char
import Data.List
import Data.Array.IO
import System.Random
import Control.Monad

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

selectRandomElem ws = do
  n <- randomRIO (1, length ws)
  return (ws !! (n-1), take (n-1) ws ++ drop n ws)

playGame ws limit = do
  printWords
  rs <- selectRandomElem ws
  playGame' rs 1
  where
    printWords = do
      putStrLn "*** Guess the word from following ones ***"
      mapM_ putStrLn ws

    playGame' rs@(w, ws') trial = do
      if trial <= limit
        then do
           putStr "Enter your guess: "
           guess <- getLine
           if w == guess
             then putStrLn $ "You got it in " ++ (show trial) ++ " trial(s)"
             else do
                if length w /= length guess
                  then do
                     putStrLn "Please enter a same length word"
                     playGame' rs trial
                  else do
                     let similarity = similarityIndex guess w
                     putStrLn $ "Word similarity is " ++ (show similarity) ++ ". Try again"
                     playGame' rs (trial + 1)
        else
           putStrLn $ "Sorry you lost... Word is: " ++ w

-- Words in the dictionary is delimited by a carriage return
-- Thus the real lengths of the words we have to find must be +1 than the
-- length provided for the following function.
-- When we return the words we strip out the '\r' character
selectWords wlen = do
  words <- readWords "Random/enable1.txt"
  return $ map init $ nLengthWords (wlen+1) words

selectRandomWords wlen num = selectWords wlen >>=  shuffle >>= return . take num

-- Let's implement a shuffle algorithm as shown in
-- http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
  arr <- newArray' n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr j vi
    return vj
  where
    n = length xs
    newArray' :: Int -> [a] -> IO (IOArray Int a)
    newArray' t = newListArray (1,t)

playGameIO' wlen count trials = selectRandomWords wlen count >>= \ws -> playGame ws trials

playGameIO :: IO ()
playGameIO = do
  putStr "Enter word length level: "
  slen <- getLine
  let len = read slen :: Int
  putStr "Enter word count: "
  scount <- getLine
  let count = read scount :: Int
  putStr "Enter trials count: "
  strials <- getLine
  let trials = read strials :: Int
  playGameIO' len count trials

main :: IO ()
main = do
  putStrLn "Starting game"
  str <- getLine
  putStrLn $ "Length is: " ++ (show $ length str)
