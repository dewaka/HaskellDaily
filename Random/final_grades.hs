-- http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/

import Data.List (words, lines, sortBy)
import Data.Char (isLetter, isAlpha, isDigit)

data Name = Name { firstName :: String
                         , lastName :: String } deriving (Show, Eq)

data ScoreRecord = ScoreRecord { name :: Name
                               , scores :: [Int]
                               } deriving (Show, Eq)

data GradeT = A | B | C | D | F deriving (Show, Eq)
data GradeSign = Plus | Minus deriving (Show, Eq)
data Grade = Grade GradeT (Maybe GradeSign) deriving (Eq)

data MarkEntry = MarkEntry { score :: (Int, Grade)
                           , record :: ScoreRecord } deriving (Show, Eq)

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

trimNonAlpha = reverse . trimPrefix . reverse . trimPrefix
  where
    trimPrefix = dropWhile (not . isAlpha)

readName :: String -> Maybe Name
readName str =
  let fname = trimNonAlpha $ takeWhile (/= ',') str
      lname = trimNonAlpha $ dropWhile (/= ',') str
  in Just $ Name fname lname

readScoreRecord :: String -> Maybe ScoreRecord
readScoreRecord str =
  let nameStr = takeWhile (not . isDigit) str
      marksStr = dropWhile (not . isDigit) str
      scores = map read $ words marksStr
  in readName nameStr >>= (\name -> return $ ScoreRecord name scores)

readScoreRecordsFromFile :: FilePath -> IO [Maybe ScoreRecord]
readScoreRecordsFromFile file =
  readFile file >>= return . lines >>= return . map readScoreRecord

markScores :: [ScoreRecord] -> [MarkEntry]
markScores rs =
  let averages = map computeAverge rs
      grades = map computeGrade rs
      entries = zipWith (\m s -> MarkEntry m s) (zip averages grades) rs
  in entries

rankScores :: [MarkEntry] -> [MarkEntry]
rankScores ms =
  sortBy (\(MarkEntry (n, _) _) (MarkEntry (m, _) _) -> (-n) `compare` (-m)) ms

printReport :: MarkEntry -> IO ()
printReport (MarkEntry (a, g) sc) = do
  putStr $ firstName (name sc) ++ " " ++ lastName (name sc)
  putStr $ " (" ++ (show a) ++ "%) " ++ (show g) ++ ": "
  mapM_ (\m -> putStr $ (show m) ++ " ") $ scores sc
  putStrLn ""

printReportFromFile file = do
  rs <- readScoreRecordsFromFile file
  let xs = rs >>= return . (\(Just x) -> x)
      ms = markScores xs
  mapM_ printReport $ rankScores ms
  return ()

exampleReport = do
  printReportFromFile "Random/final_grades_input.txt"

