import Control.Monad.State

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score
playGame (x:xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on, score + 1)
    'b' | on -> put (on, score - 1)
    'c'      -> put (not on, score)
    _        -> put (on, score)
  playGame xs

startState = (False, 0)

main = print $ evalState (playGame "abcaaacbbcabbab") startState

type FibState = (Int, Int)
type FibValue = Int

fib :: Int -> State FibState FibValue
fib n
  | n == 0 || n == 1 = do
    (_, n) <- get
    return n
  | otherwise = do
    (a, b) <- get
    put (b, a+b)
    fib (n-1)

{-
At the beginning, Marvin takes N marbles (where N is a nonnegative integer) and
places them into bag 0. The remaining four bags are left empty.
Marvin then follows this simple algorithm:

    Add a marble into bag 1.
    Repeat forever:
        Add a marble into bag 1.
        Empty bag 4.
        While there are marbles in bag 0:
            While there are marbles both in bag 0 and in bag 1:
                Remove a marble from bag 0.
                Remove a marble from bag 1.
                Add a marble into bag 2.
                Add a marble into bag 3.
            End While
            Add a marble into bag 4.
            If bags 0 and 1 are both empty:
                Move all marbles from bag 3 to bag 4.
                TERMINATE THE GAME
            End If
            Move all marbles from bag 3 to bag 1.
        End While
        Move all marbles from bag 2 to bag 0.
    End Repeat
-}

type MState = (Int, Int, Int, Int, Int, Int)
type MValue = Int

initMState = (0, 0, 0, 0, 0, 0) :: MState

playMarvin :: Int -> State MState MValue
playMarvin n = do
  put (1, n, 0, 0, 0, 0)
  put (2, n, 1, 0, 0, 0)
  play
  where
    play = do
      (n, b0, b1, b2, b3, b4) <- get
      put (n+1, b0, b1+1, b2, b3, 0)
      loop1
      (n, b0, b1, b2, b3, b4) <- get
      return n

    loop1 = do
      (n, b0, b1, b2, b3, b4) <- get
      if b0 > 0
        then undefined
        else undefined
