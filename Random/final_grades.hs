-- http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/

newtype FullName = FullName (String, String) deriving (Show, Eq)

data ScoreRecord = ScoreRecord { name :: FullName
                               , scores :: [Int]
                               } deriving (Show, Eq)

computeAverge :: ScoreRecord -> Int
computeAverge ScoreRecord { scores = sc } = round $ sum_scores / n
  where
    sum_scores = fromIntegral $ sum sc
    n = fromIntegral $ length sc


