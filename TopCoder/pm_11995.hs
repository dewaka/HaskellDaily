-- http://community.topcoder.com/stat?c=problem_statement&pm=11995

-- Problem: Checker Expansion

{-
Problem Statement

Alice and Bob are playing a game on an infinite grid. The grid is initially empty.
Alice and Bob each have an infinite supply of checkers. Alice's checkers are red, Bob's are blue.
In the first turn, Alice places one of her red checkers into the cell (0,0).
The game continues by Bob and Alice taking alternate turns. In each turn, the current player
places their checkers into some empty cells. The player must always add all the checkers
that can be added in their turn by repeatedly applying the following rules:

If cell (x-1,y-1) contains a checker of the other player and cell (x-2,y) is empty,
add your checker into the cell (x,y).
If cell (x-2,y) contains a checker of the other player and cell (x-1,y-1) is empty,
add your checker into the cell (x,y).

Bob has asked you to simulate the game so that he can understand it better.
You are given longs t, x0 and y0 and ints w and h. Return a String[] containing h elements
of w characters each. The j-th character of the i-th element of your return value will
represent the contents of cell at x=x0+j and y=y0+h-i-1 after t turns. The character
representing a particular cell should be 'A' (quotes for clarity) if it contains one
of Alice's checkers, 'B' if it contains one of Bob's checkers and '.' if it is empty.
-}

import Data.List (foldl')

data Checker = A | B | E deriving (Eq)

instance Show Checker where
  show A = "A"
  show B = "B"
  show E = "."

type Pos = (Int, Int)

type Board = [(Pos, Checker)]

position :: Board -> Pos -> Checker
position board (x, y) =
  let p = filter (\((m, n), _) -> x==m && y==n) board
  in if p == [] then E else snd $ head p

emptyBoard :: Board -> Bool
emptyBoard = null

boardMaxX :: Board -> Int
boardMaxX = maximum . (map (fst . fst))

boardMaxY :: Board -> Int
boardMaxY = maximum . (map (snd . fst))

otherChecker A = B
otherChecker B = A

canPlay :: Board -> Checker -> Pos -> Bool
canPlay _ E _ = error "Cannot play empty checker"
canPlay [] _ _ = True
canPlay board c (x, y) = ruleOne || ruleTwo
  where
    c' = otherChecker c
    (x_1, y_1, x_2) = (x - 1, y - 1, x - 2)
    ruleOne = position board (x_1, y_1) == c' && position board (x_2, y) == E
    ruleTwo = position board (x_2, y) == c' && position board (x_1, y_1) == E

startBoard :: Board
startBoard = [((0, 0), A)]      -- At the start Alice plays first on (0, 0)

possiblePositions :: Board -> Checker -> [Pos]
possiblePositions board c =
  let m = max (boardMaxX board) (boardMaxY board)
      ps = [-(m+2)..(m+2)]
      posList = [(x, y) | x <- ps, y <- ps]
  in filter (canPlay board c) posList

playNext :: Board -> Checker -> Board
playNext [] c = [((0, 0), c)]
playNext board c = board ++ newPositions
  where
    newPositions = map (\p -> (p, c)) $ possiblePositions board c

play :: Board -> Int -> Board
play board n = foldl' playNext board $ take n $ cycle [A, B]

play' = play []

displayResult board (x0, y0) (w, h) =
  mapM_ displayRow [[(x0+a, y0+b) | a <- [0..w-1]] | b <- [0..h-1]]
  where
    displayRow row = do
      mapM_ (putStr . show . position board) row
      putStrLn ""

answer t x0 y0 w h = do
  let board = play' t
  displayResult board (x0, y0) (w, h)

main :: IO ()
main = do
  putStrLn "*** Checker Expansion ***"
