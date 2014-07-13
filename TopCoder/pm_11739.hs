-- http://community.topcoder.com/stat?c=problem_statement&pm=11739

{-

Problem Statement : MinskyMystery

Marvin plays a simple game. The game is played with an infinite supply of
marbles and five bags, labeled "bag 0" through "bag 4".

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

You are given a long N. Count the number of times a marble enters a bag
during Marvin's game. That is, compute X+Y, where X is the number of times a
marble is added to some bag, and Y is the number of times a marble is moved from
one bag to another. To avoid overflows, return just the value
((X+Y) modulo 1,000,000,009). If Marvin's game does not
terminate for the given N, return -1 instead.
-}

import Control.Monad.State

data Bag = Bag String Int deriving (Show, Eq)

bagEmpty (Bag _ 0) = True
bagEmpty _        = False

emptyBag name = Bag name 0

addToBag (Bag s n) m = Bag s (m+n)

main :: IO ()
main = do
  putStrLn "*** MinskyMystery Solution ***"
