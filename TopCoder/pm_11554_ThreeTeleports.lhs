http://community.topcoder.com/stat?c=problem_statement&pm=11554

You are a frog. You live on an infinite lattice of grid points. For each pair of
integers x, y there is a grid point with coordinates (x,y). At this moment, you
sit on the grid point (xMe,yMe). You want to get home: to the grid
point (xHome,yHome).

There are two ways in which you can travel. Your first option is jumping: if you
are at (x,y), you can jump to one of the four neighboring grid points: (x+1,y),
(x-1,y), (x,y+1), or (x,y-1). Each jump takes one second.

Your second option is using a teleport. There are three teleports in the entire
world. Each of them connects two different grid points. If you are at one of the
endpoints, you may activate the teleport and travel to its other endpoint.
Traveling by teleport takes 10 seconds.

You are given ints xMe, yMe, xHome, yHome, and a String[] teleports that
describes the three teleports. Each element of teleports will be a String
containing four integers x1, y1, x2, and y2, separated by single spaces.
These integers describe a teleport with endpoints at (x1,y1) and (x2,y2).

Your method must return the shortest time in which you can reach your home.

> type Point = (Int, Int)
> data Teleport = Teleport { start :: Point
>                          , end :: Point
>                          } deriving (Show, Eq)
> data GridPoint = GPoint Point | GTeleport Teleport

> timeBetween :: Point -> Point -> Int
> timeBetween (m, n) (x, y) = abs (m-x) + abs (n-y)

> totalTime (GPoint p1:GPoint p2:xs) = timeBetween p1 p2 + totalTime xs
> totalTime (GPoint p:GTeleport tp:xs) =
>   timeBetween p (start tp) + 10 + totalTime ((GPoint $ end tp):xs)
> totalTime [] = 0
> totalTime _ = error "Unexpected calculation"

> timeToTravel :: Point -> Point -> [Teleport] -> Int
> timeToTravel sPoint ePoint tps =
>   totalTime $ [GPoint sPoint] ++ map GTeleport tps ++ [GPoint ePoint]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ThreeTeleports ***"
>   answer

> answer = undefined
