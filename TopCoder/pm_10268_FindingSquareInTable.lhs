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

0)	
    	
{"123",
 "456"}
Returns: 64
Several three-digit numbers can be obtained: 123, 321, 456, and 654, but none of them is a perfect square. The largest obtainable perfect square is 64.
1)	
    	
{"00000",
 "00000",
 "00200",
 "00000",
 "00000"}
Returns: 0
0 is a perfect square. It is the largest one that can be obtained in this table.
2)	
    	
{"3791178",
 "1283252",
 "4103617",
 "8233494",
 "8725572",
 "2937261"}
Returns: 320356
Take the i-th digit of each row i, and you'll get 320356 = 566 × 566.
3)	
    	
{"135791357",
 "357913579",
 "579135791",
 "791357913",
 "913579135"}
Returns: 9
It is known that a perfect square can't end with two odd digits. Therefore, the largest perfect square that contains only odd digits is 9.
4)	
    	
{"553333733",
 "775337775",
 "777537775",
 "777357333",
 "755553557",
 "355533335",
 "373773573",
 "337373777",
 "775557777"}
Returns: -1
There exists no perfect square that contains only digits 3, 5, and 7.
5)	
    	
{"257240281",
 "197510846",
 "014345401",
 "035562575",
 "974935632",
 "865865933",
 "684684987",
 "768934659",
 "287493867"}

> perfectSquare :: Integral a => a -> Bool
> perfectSquare num = num == n*n
>   where
>     n = floor $ sqrt $ fromIntegral num

> main :: IO ()
> main = do
>   putStrLn "*** Solution to FindingSquareInTable ***"
>   answer

> answer = undefined
