http://www.reddit.com/r/dailyprogrammer/comments/126905/10272012_challenge_108_intermediate_minesweeper/

Generating Minesweeper boards

> data MSquare = Mine | NonMine Int deriving (Show, Eq)

> type Pos = (Int, Int)
> data Board = Board { height :: Int
>                    , width :: Int
>                    , cells :: [[MSquare]]
>                    } deriving (Show)

> adjacentMineCount :: Board -> Pos -> Int
> adjacentMineCount b (x,y) = sum $ map count adjCells
>   where
>     adjCells = [(x+i,y+j) | i <- [-1..1], j <- [-1..1],
>                 0 <= x+i && x+i < width b,
>                 0 <= y+j && y+j < height b]
>
>     count (x, y) = if (cells b !! y) !! x == Mine
>                    then 1
>                    else 0

As a convention NonMine (-1) represents a cell which has not been updated
to reflect the actual mine count surrounding that particular cell

> sampleBoard = Board { height = 3
>                     , width = 3
>                     , cells =  [ [Mine, NonMine (-1), Mine]
>                                , [Mine, NonMine (-1), Mine]
>                                , [Mine, NonMine (-1), Mine]
>                                ]
>                     }

> printBoard :: Board -> IO ()
> printBoard b = mapM_ print $ cells b

This is an extremely ugly method to update the board.
Better way to do this would be to not to use lists at all!
Using a Vector to represent the Board cells would be much better.

> updateAdjacentMineCount :: Board -> Pos -> Board
> updateAdjacentMineCount b (x,y) = if cell == Mine
>                                   then b
>                                   else b { cells = updatedCells }
>   where
>     cells' = cells b
>     row = cells' !! y
>     cell = (cells' !! y) !! x
>     n = adjacentMineCount b (x,y)
>     newRow = take x row ++ [NonMine n] ++ drop (x+1) row
>     updatedCells = take y cells' ++ [newRow] ++ drop (y+1) cells'

> updateBoard :: Board -> Board
> updateBoard b =
>   foldl updateAdjacentMineCount b [(x, y) | x <- [0..width b-1], y <- [0..height b-1]]

> main :: IO ()
> main = do
>   putStrLn "Starting Minesweeper Game"
>   printBoard sampleBoard
>   putStrLn "Updated Board with Correct Mine Counts"
>   printBoard $ updateBoard sampleBoard
