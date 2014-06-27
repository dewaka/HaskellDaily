-- http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/

import Data.List (words, lines)

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

readScoreRecordsFromFile :: FilePath -> IO [Maybe ScoreRecord]
readScoreRecordsFromFile file =
  readFile file >>= return . lines >>= return . map readScoreRecord

-- Test data
vetter = ScoreRecord { name = Name "Valerie" "Vetter"
                     , scores = [79, 81, 78, 83, 80] }

richie = ScoreRecord { name = Name "Richie" "Rich"
                     , scores = [88, 90, 87, 91, 86] }
