Problem Statement: FindingSquareInTable

You are given a String[] table representing a rectangular grid where each cell
contains a digit. The j-th character of the i-th element of table is the digit
in the cell at row i, column j of the grid.

Consider a sequence of distinct cells in this table where the row numbers of the
cells (in the order they appear in the sequence) form an arithmetic progression,
and the column numbers also form an arithmetic progression. Concatenate the
digits in the cells of this sequence to obtain a number.

Among all numbers that can be obtained in this manner, find and return the
largest perfect square (a square of an integer). If there are no perfect
squares, return -1 instead.

> perfectSquare :: Integral a => a -> Bool
> perfectSquare num = num == n*n
>   where
>     n = floor $ sqrt $ fromIntegral num

> main :: IO ()
> main = do
>   putStrLn "*** Solution to FindingSquareInTable ***"
>   answer

> answer = undefined
