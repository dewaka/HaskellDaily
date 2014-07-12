-- http://community.topcoder.com/stat?c=problem_statement&pm=12180

{-
Fox Jiro has a rectangular grid with H rows and W columns (i.e., the grid has H*W cells in total).
Initially, each cell in the grid contained the character '0'.

A row flip is an operation in which Jiro picks a row of the grid, and in that row he changes
all '0's to '1's and vice versa. Similarly, a column flip is an operation in which Jiro
does the same to a column of the grid. Jiro took the grid that contained '0's everywhere.
He performed a row flip Rcount times, and then a column flip Ccount times.
(It is possible that Jiro flipped the same row or column multiple times.)
At the end, Jiro noticed that there are exactly S '1's in the grid.

You are given the ints H, W, Rcount, Ccount, and S. We are interested in the number of
different ways in which Jiro could have flipped the rows and columns of the grid.
Two ways of flipping are considered different if there is a row or a column that was flipped a
different number of times. (That is, the order in which the rows and columns are
flipped does not matter.) Return the number of different ways of flipping that match the
given situation, modulo 555,555,555.
-}

import Data.Bits (complement)
import Data.List (tails)

-- Classic combinations
combinations 0 _  = [[]]
combinations n xs = [y:ys' | y:xs' <- tails xs
                           , ys' <- combinations (n-1) xs']

-- Combinations with replacements
combinationsWR 0 _  = [[]]
combinationsWR n xs = [y:ys' | y:xs' <- tails xs
                             , ys' <- combinationsWR (n-1) xs]

genGrid h w = take h $ repeat row
  where
    row = take w $ repeat 0

flipNum x = if x==0 then 1 else 0

flipRow n tb = undefined

main :: IO ()
main = do
  putStrLn "*** XorBoard Solution ***"
