http://community.topcoder.com/stat?c=problem_statement&pm=5895

You are on vacation on a tropical island, but you couldn't resist the temptation
of solving a good old problem. It all started when a group of kids played a game
they call "The Falling Coconuts". In this game, a number of coconuts fall to the
ground, one by one, on a single axis, at the locations given in drops.
If a coconut X lands on the ground, it remains where it is. If it lands on top
of another coconut Y, one of the following things happens:

- If coconut Y is surrounded on both sides by coconuts (denoted by 'O'), coconut
X remains where it is.
     X
    OYO
- If there is no coconut directly to the right of coconut Y, coconut X slides
down to the position directly to the right of coconut Y.
     X
    OY   ->  OYX
     X
     Y   ->   YX
- If there is a coconut directly to the right of coconut Y, but no coconut
directly to the left of coconut Y, coconut X slides down to the position directly
to the left of coconut Y.
     X
     YO  ->  XYO


Each time coconut X slides down to a different position, it will continue to
slide (following the behavior outlined above) until it's in a place where it
will not slide any further.

The task is to return the final coconut configuration in a String[]. Each
element represents a single row in the final configuration. The first element
represents the lowest row and the last element represents the highest row.
Within each element, coconuts are represented by the uppercase letter 'O', and
empty space is represented by '-'. The first and last characters of the first
element of the String[] must both be 'O', and the rest of the elements must have
exactly the same number of characters as the first. Each element included in the
String[] must contain at least one 'O' character.

> import Data.List (sortBy)

> type Pos = Int
> type Board = [(Pos, Pos)]

> vacant board n = not $ n `elem` board
> left (x, y) = (x-1, y)
> right (x, y) = (x+1, y)

> addToBoard :: Board -> (Pos, Pos) -> Board
> addToBoard board n@(x, y) = if n `elem` board
>                             then if vacant board $ right n
>                                  then right n:board
>                                  else if vacant board $ left n
>                                       then left n:board
>                                       else addToBoard board (x, y+1)
>                             else n:board

> addAllToBoard board pos =
>   foldl addToBoard board $ zip pos $ repeat 0

> orderBoard :: Board -> Board
> orderBoard = sortBy comparePos
>   where
>     comparePos (x1,y1) (x2,y2) = case y1 `compare` y2 of
>                                   EQ -> x1 `compare` x2
>                                   e -> e

> boardLayers :: Board -> [Board]
> boardLayers board = go m
>   where
>     go i
>       | i > n = []
>       | otherwise = filter ((==i) . snd) board : go (i+1)
>     m = minimum $ map snd board
>     n = maximum $ map snd board

> addAllToBoard' b ps = orderBoard $ addAllToBoard b ps

> layerToStr :: Board -> Pos -> String
> layerToStr board t = map posToChar [(x, t) | x <- [m..n]]
>   where
>     tlayer = filter ((==t) . snd) board
>     m = minimum $ map fst tlayer
>     n = maximum $ map fst tlayer
>     posToChar p = if p `elem` board then 'O' else '-'

> shiftByMinX :: Board -> Board
> shiftByMinX board = map (\(x,y) -> (x-m,y)) board
>   where
>     m = minimum $ map fst board

> example1 = addAllToBoard' [] [8,9,10,11,12,8,12,10]
> example2 = addAllToBoard' [] [5,20,5,20,5,6,7]
> example3 = addAllToBoard' [] [6,8,10,7,9,8,8,8,8,8]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to Over9000Rocks ***"
>   answer

> answer = undefined
