-- http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/

type FullName = (String, String)

data ScoreRecord = ScoreRecord { name :: FullName
                               , scores :: [Int]
                               } deriving (Show, Eq)

computeAverge :: ScoreRecord -> Int
computeAverge ScoreRecord { scores = sc } = round $ sum_scores / n
  where
    sum_scores = fromIntegral $ sum sc
    n = fromIntegral $ length sc


vetter = ScoreRecord { name = ("Valerie", "Vetter")
                     , scores = [79, 81, 78, 83, 80] }

richie = ScoreRecord { name = ("Richie", "Rich")
                     , scores = [88, 90, 87, 91, 86] }
