-- Rock Paper Scissors Lizard Spock
-- http://www.reddit.com/r/dailyprogrammer/comments/23lfrf/4212014_challenge_159_easy_rock_paper_scissors/

import System.Random
import Data.Char
import qualified Data.Map as M

data Move = Rock | Paper | Scissors | Lizard | Spock deriving (Ord, Eq, Show, Read)

data Player = Human | Computer deriving (Show)

data GameResult = Win Player | Draw deriving (Show)

-- The rules of rock-paper-scissors-lizard-Spock are:
-- Scissors cut paper
-- Paper covers rock
-- Rock crushes lizard
-- Lizard poisons Spock
-- Spock smashes scissors
-- Scissors decapitate lizard
-- Lizard eats paper
-- Paper disproves Spock
-- Spock vaporizes rock
-- Rock crushes scissors

winningMap :: M.Map (Move, Move) String
winningMap = M.fromList $ map (\(m1, s, m2) -> ((m1, m2), s)) [ (Scissors, "cut", Paper)
                                                              , (Paper, "covers", Rock)
                                                              , (Rock, "crushes", Lizard)
                                                              , (Lizard, "poisons", Spock)
                                                              , (Spock, "smashes", Scissors)
                                                              , (Scissors, "decapitate", Lizard)
                                                              , (Lizard, "eats", Paper)
                                                              , (Paper, "disproves", Spock)
                                                              , (Spock, "vaporizes", Rock)
                                                              , (Rock, "crushes", Scissors)
                                                              ]

evalMoves :: Move -> Move -> GameResult
evalMoves hMove cMove =
  if hMove == cMove
  then Draw
  else case M.member (hMove, cMove) winningMap of
    True -> Win Human
    False -> Win Computer

readMove = do
  putStr "Enter move: "
  input <- getLine
  if input == "quit"
    then return $ Nothing
    else return $ Just (read input :: Move)

humanVsHumanGame = do
  putStrLn "New game. Enter quit to exit or moves to play on."
  move1 <- readMove
  case move1 of
    Nothing -> putStrLn "Bye..."
    Just m1 -> do
      move2 <- readMove
      case move2 of
        Nothing -> putStrLn "Bye..."
        Just m2 -> case evalMoves m1 m2 of
          Draw -> do
            putStrLn "It is a draw!"
            humanVsHumanGame
          Win Human -> do
            putStrLn "First player wins!"
            humanVsHumanGame
          Win Computer -> do
            putStrLn "Second player wins!"
            humanVsHumanGame

startsWith _ [] = False
startsWith x (y:_) = x == y

fuzzyMove s = map fst $ filter ((startsWith $ toLower s) . snd) moves
  where
    moves' = [Rock, Paper, Scissors, Lizard, Spock]
    moves = zip moves' (map (map toLower . show) moves')


randomMove = do
  n <- randomRIO (1, length moves)
  return $ moves !! (n-1)
  where
    moves = [Rock, Paper, Scissors, Lizard, Spock]

humanVsComputer = play 0 0 0
  where
    play wins losses draws = do
      putStrLn "New game. Enter quit to exit or moves to play on."
      move1 <- readMove
      case move1 of
        Nothing -> do
          if wins > 0 || losses > 0 || draws > 0
            then putStrLn $ "Wins: " ++ (show wins) ++ ", Losses: " ++ (show losses) ++ ", Draws: " ++ (show draws)
            else putStrLn "Bye..."
        Just m1 -> do
          m2 <- randomMove
          putStrLn $ "Computer played: " ++ (show m2)
          case evalMoves m1 m2 of
            Draw -> do
              putStrLn "It is a draw!"
              play wins losses (draws + 1)
            Win Human -> do
              putStrLn "You won!"
              play (wins + 1) losses draws
            Win Computer -> do
              putStrLn "Computer won!"
              play wins (losses + 1) draws
