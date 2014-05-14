-- Rock Paper Scissors Lizard Spock
-- http://www.reddit.com/r/dailyprogrammer/comments/23lfrf/4212014_challenge_159_easy_rock_paper_scissors/

data Move = Rock | Paper | Scissors | Lizard | Spock deriving (Show, Read)

readMove = do
  putStr "Enter move: "
  input <- getLine
  return $ (read input :: Move)



