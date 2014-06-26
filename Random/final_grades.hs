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

-- Marking Scores
-- 90-100 A
-- 80-89 B
-- 70-79 C
-- 60-69 D
-- 59 and below F
computeGrade :: ScoreRecord -> Grade
computeGrade record = Grade (grade score) Nothing
  where
    grade n
      | 100 >= n && n >= 90 = A
      | 89 >= n && n >= 80 = B
      | 79 >= n && n >= 70 = C
      | 69 >= n && n >= 60 = D
      | 59 >= 0 && n >= 0 = F
      | otherwise = error $ "Invalid score: " ++ show n

    score = computeAverge record

vetter = ScoreRecord { name = ("Valerie", "Vetter")
                     , scores = [79, 81, 78, 83, 80] }

richie = ScoreRecord { name = ("Richie", "Rich")
                     , scores = [88, 90, 87, 91, 86] }
