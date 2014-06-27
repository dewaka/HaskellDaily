-- http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/

import Data.List (words)

data Name = Name { firstName :: String
                         , lastName :: String } deriving (Show, Eq)

data ScoreRecord = ScoreRecord { name :: Name
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
computeGrade record = grade $ fromIntegral score
  where
    grade n
      | 100 >= n && n >= 90 = let sign = case calculateSign n 100 90 of
                                           Just Plus -> Nothing
                                           s -> s
                              in Grade A sign
      | 89 >= n && n >= 80 = Grade B $ calculateSign n 89 80
      | 79 >= n && n >= 70 = Grade C $ calculateSign n 79 70
      | 69 >= n && n >= 60 = Grade D $ calculateSign n 69 60
      | 59 >= 0 && n >= 0 = Grade F Nothing
      | otherwise = error $ "Invalid score: " ++ show n

    score = computeAverge record

-- readScoreRecord :: String -> Maybe ScoreRecord
readScoreRecord str =
  let ws = words str
  in if length ws >= 4
     then let (fname, lname) = (ws !! 0, ws !! 2)
              marks = map read $ drop 3 ws
          in Just $ ScoreRecord { name = Name fname lname, scores = marks }
     else Nothing

-- Test data
vetter = ScoreRecord { name = Name "Valerie" "Vetter"
                     , scores = [79, 81, 78, 83, 80] }

richie = ScoreRecord { name = Name "Richie" "Rich"
                     , scores = [88, 90, 87, 91, 86] }

-- Challenge input
{-
Jennifer ,  Adams   100 70  85  86  79
Bubba , Bo Bob  50  55  60  53  30
Matt ,  Brown   72  82  92  88  79
Ned ,   Bundy   73  75  80  79  88
Alfred ,    Butler  80  90  70  100 60
Sarah , Cortez  90  72  61  70  80
William ,   Fence   88  86  83  70  79
Casper ,    Ghost   80  85  87  89  90
Opie ,  Griffith    90  90  90  90  90
Tony ,  Hawk    60  60  60  72  72
Kirstin ,   Hill    100 90  92  94  95
Hodor , Hodor   40  50  53  62  33
Clark , Kent    89  90  88  92  91
Tyrion ,    Lannister   93  97  100 91  95
Ken ,   Larson  70  80  85  73  79
Stannis ,   Mannis  60  70  75  77  78
Bob ,   Martinez    79  88  92  82  72
Jean Luc ,  Picard  90  89  95  70  65
Harry , Potter  73  75  77  69  73
Jaina , Proudmoore  90  92  100 95  94
Richie ,    Rich    88  90  87  91  86
John ,  Smith   90  80  70  60  50
Jon ,   Snow    70  70  70  70  72
Arya ,  Stark   91  92  90  93  90
Edwin , Van Clef    40  50  55  57  33
Valerie ,   Vetter  79  81  78  83  80
Katelyn ,   Weekes  90  95  92  93  97
Wil  , Wheaton  70  80  75  71  77
Steve , Wozniak 88  89  87  86  85
Derek , Zoolander   80  81  85  88  90
-}
