http://community.topcoder.com/stat?c=problem_statement&pm=3972

Problem Statement: WordFind

You have been given a "word search" puzzle, which consists of a rectangular grid
of letters, in which several words are hidden. Each word may begin anywhere in
the puzzle, and may be oriented in any straight line horizontally, vertically,
or diagonally. However, the words must all go down, right, or down-right.

You are given a String[], grid, indicating the letters in the grid to be searched.
Character j of element i of grid is the letter at row i, column j. You are also
given a String[], wordList, indicating the words to be found in the grid.
You are to return a String[] indicating the locations of each word within the grid.

The return value should have the same number of elements as wordList.
Each element of wordList corresponds to the element of the return value with the
same index.

Each element of the return value should be formatted as "row col" (quotes added
for clarity), where row is the 0-based row in which the first letter of the word
is found, and col is the 0-based column in which the first letter of the word is
found. If the same word can be found more than once, the location in the
lowest-indexed row should be returned. If there is still a tie, return the
location with the lowest-indexed column. If a word cannot be found in the grid,
return an empty string for that element.

> import Data.Maybe (isJust)

> type Puzzle = [((Int, Int), Char)]

> toPuzzle :: [String] -> Puzzle
> toPuzzle xs = foldl go [] $ zip [0..] xs
>   where
>     go acc (r, ys) =  acc ++ map (\(c, y) -> ((r, c), y)) (zip [0..] ys)

> startsWith (x:xs) (y:ys) = x==y && startsWith xs ys
> startsWith [] _ = True
> startsWith _ [] = False

> subPosition needle haystack = go haystack 0
>   where
>     go [] _ = Nothing
>     go xs@(_:xs') n = if startsWith needle xs
>                       then Just n
>                       else go xs' (n+1)

> findDown pz needle =
>   let (rows, columns) = getBounds pz
>       strings = traverse pz $ [[(i, j) | i <- [0..rows]] | j <- [0..columns]]
>   in filter isJust [subPosition needle s | s <- strings]

> findRight pz needle =
>   let (rows, columns) = getBounds pz
>       strings = traverse pz $ [[(i, j) | j <- [0..rows]] | i <- [0..columns]]
>   in filter isJust [subPosition needle s | s <- strings]

> findDiagonal pz needle =
>   let (rows, columns) = getBounds pz
>       strings = traverse pz $
>                 [[(i+j, 0+j) | j <- [0..columns], i+j <=rows] | i <- [0..rows]]
>                 ++ [[(0+j, i+j) | j <- [0..columns], i+j <=columns] | i <- [1..rows]]
>   in filter isJust [subPosition needle s | s <- strings]

> getBounds pz =
>   let rows = maximum $ map (\((r, _), _) -> r) pz
>       columns = maximum $ map (\((_, c), _) -> c) pz
>   in (rows, columns)

> traverse :: Puzzle -> [[(Int, Int)]] -> [String]
> traverse pz prows = map (map (\(Just s) -> s) . filter isJust)
>                     $ [[ lookup p pz | p <- points ] | points <- prows ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to WordFind ***"
>   answer

> answer = undefined

> example1 = toPuzzle ["abc", "def", "ghi"]
> example2 = toPuzzle ["abcdef", "ghijkl"]
