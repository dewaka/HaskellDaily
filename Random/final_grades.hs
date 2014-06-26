-- http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/

type FullName = (String, String)

data ScoreRecord = ScoreRecord { name :: FullName
                               , scores :: [Int]
                               } deriving (Show, Eq)

data GradeT = A | B | C | D | F deriving (Show, Eq)
data GradeSign = Plus | Minus deriving (Show, Eq)
data Grade = Grade GradeT (Maybe GradeSign) deriving (Eq)

instance Show Grade where
  show (Grade g Nothing) = show g
  show (Grade g (Just Plus)) = show g ++ "+"
  show (Grade g (Just Minus)) = show g ++ "-"

computeAverge :: ScoreRecord -> Int
computeAverge ScoreRecord { scores = sc } = round $ sum_scores / n
  where
    sum_scores = fromIntegral $ sum sc
    n = fromIntegral $ length sc

calculateSign mark upper lower =
  if mark > mid
  then test mark upper Plus
  else test mark lower Minus
  where
    test m u s = if abs ((u - m) / 100) <= cutoff
                 then Just s
                 else Nothing
    mid = lower + (upper - lower) / 2
    cutoff = 0.03

-- Marking Scores
computeGrade :: ScoreRecord -> Grade
computeGrade record = Grade g Nothing
  where
    (g, _) = grade $ fromIntegral score

    grade n
      | 100 >= n && n >= 90 = (A, (n - 90) / 100)
      | 89 >= n && n >= 80 = (B, (n - 80) / 100)
      | 79 >= n && n >= 70 = (C, (n - 70) / 100)
      | 69 >= n && n >= 60 = (D, (n - 60) / 100)
      | 59 >= 0 && n >= 0 = (F, 0/100)
      | otherwise = error $ "Invalid score: " ++ show n

    score = computeAverge record


vetter = ScoreRecord { name = ("Valerie", "Vetter")
                     , scores = [79, 81, 78, 83, 80] }

richie = ScoreRecord { name = ("Richie", "Rich")
                     , scores = [88, 90, 87, 91, 86] }
