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

> import Data.List (tails)

> type Point = (Int, Int)

> data Teleport = Teleport { start :: Point
>                          , end :: Point
>                          } deriving (Eq)

> instance Show Teleport where
>   show (Teleport s e) = "TP{" ++ (show s) ++ "->" ++ (show e) ++ "}"

> data GridPoint = GPoint Point | GTeleport Teleport

> data Path = Path { startPoint :: Point
>                  , endPoint :: Point
>                  , teleports :: [Teleport]
>                  } deriving (Show, Eq)

> selections [] = []
> selections (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- selections xs]

> permutations [] = [[]]
> permutations xs = [ y:zs | (y,ys) <- selections xs
>                          , zs <- permutations ys ]

> combinations 0 _ = [[]]
> combinations n xs = [ y:zs | (y:ys) <- tails xs
>                            , zs <- combinations (n-1) ys ]

> selectPermutations n xs = concat [ permutations ys | ys <- combinations n xs ]

> timeBetween :: Point -> Point -> Int
> timeBetween (m, n) (x, y) = abs (m-x) + abs (n-y)

> totalTime (GPoint p1:GPoint p2:xs) = timeBetween p1 p2 + totalTime xs
> totalTime (GPoint p:GTeleport tp:xs) =
>   timeBetween p (start tp) + 10 + totalTime ((GPoint $ end tp):xs)
> totalTime [] = 0
> totalTime _ = error "Unexpected calculation"

> pathTime (Path s e tp) = timeToTravel s e tp
>   where
>     timeToTravel sPoint ePoint tps =
>       totalTime $ [GPoint sPoint] ++ map GTeleport tps ++ [GPoint ePoint]

> allPossiblePaths sp ep tps = [Path sp ep ts | n <- [0..length tps]
>                                             , ts <- selectPermutations n tps]

> allTimes s e tps = map pathTime $ allPossiblePaths s e tps

I had to modify how to teleports are used since I realized very late that the
direction they are used does not matter. So, instead of modifying the calculation
logic I read it as if a teleportation string will yield not just one but two
distinct teleportations. This works just fine for calculation logic.

> readTeleport stel =
>   let [x1, y1, x2, y2] = words stel
>       s = (read x1, read y1)
>       e = (read x2, read y2)
>   in [Teleport s e, Teleport e s]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ThreeTeleports ***"
>   answer

Minimum time selection is given in the local function calcMin. This can be easily
modified to return the minimum cost Path as well.

> answer = mapM_ (print . calcMin) examples
>   where
>     calcMin (s, e, ts) =
>       let [t1s, t2s, t3s] = map readTeleport ts
>       in minimum $ concat [allTimes s e [t1, t2, t3] | t1 <- t1s
>                                                      , t2 <- t2s
>                                                      , t3 <- t3s]
>     examples = [ ( (3,3), (4,5), [ "1000 1001 1000 1002"
>                                  , "1000 1003 1000 1004"
>                                  , "1000 1005 1000 1006" ] )
>                , ( (0,0), (20,20), [ "1 1 18 20"
>                                     , "1000 1003 1000 1004"
>                                     , "1000 1005 1000 1006" ] )
>                , ( (0,0), (20,20), [ "1000 1003 1000 1004"
>                                    , "18 20 1 1"
>                                    , "1000 1005 1000 1006" ] )
>                , ( (3,7), (10000,30000), [ "3 10 5200 4900"
>                                          , "12212 8699 9999 30011"
>                                          , "12200 8701 5203 4845" ] )
>                , ( (0, 0), (1000000000, 1000000000), [ "0 1 0 999999999"
>                                                      , "1 1000000000 999999999 0"
>                                                      , "1000000000 1 1000000000 999999999" ] )
>                ]
