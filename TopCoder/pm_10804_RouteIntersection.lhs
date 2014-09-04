http://community.topcoder.com/stat?c=problem_statement&pm=10804

> import Data.List (sort)

> data Point = Zero { numDim :: Int }
>            | Point { numDim :: Int, dimensions :: [(Int, Int)] }
>            deriving (Show)

> isZero :: Point -> Bool
> isZero (Zero _) = True
> isZero (Point _ dz) = all ((==0) . snd) dz

> nonZeroDimensions (Zero _) = []
> nonZeroDimensions (Point _ dz) = filter ((/=0) . snd) dz

> instance Eq Point where
>   (Zero m) == (Zero n) = m == n
>   p == (Zero n) = numDim p == n && isZero p
>   (Zero n) == p = numDim p == n && isZero p
>   p1 == p2 = p1dz == p2dz
>     where
>       p1dz = sort $ nonZeroDimensions p1
>       p2dz = sort $ nonZeroDimensions p2

> update d x f [] = [(x, f d)]
> update d x f (t@(y, v):xs)
>   | x == y = (x, f v) : xs
>   | otherwise = t : update d x f xs

> updateDimension (Zero n) dim f
>   | 0 < dim && dim <= n = Point n $ update 0 dim f []
>   | otherwise = error "Invalid dimension"
> updateDimension (Point n dz) dim f
>   | 0 < dim && dim <= n = Point n $ update 0 dim f dz
>   | otherwise = error "Invalid dimension"

> unique [] = True
> unique (x:xs)
>   | x `elem` xs = False
>   | otherwise = unique xs

> journey :: Int -> [Point] -> [(Int, Char)] -> [Point]
> journey mov = foldl go
>   where
>     go ps@(last:_) (n, '+') = updateDimension last n (+mov):ps
>     go ps@(last:_) (n, '-') = updateDimension last n (\t -> t - mov):ps

> dazJourney dim dz dirs = journey 1 [Zero dim] $ zip dz dirs

> uniqueDazJourney dim dz dirs = unique $ dazJourney dim dz dirs

> main :: IO ()
> main = do
>   putStrLn "*** Solution to RouteIntersection ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go (dim, dz, dirs) = if uniqueDazJourney dim dz dirs
>                          then "VALID"
>                          else "NOT VALID"
>     examples = [ (1, [1], "+")
>                , (2, [1,2,1,2], "++--")
>                , (3, [1,2,3,1,2], "+++--")
>                , (344447,
>                   [132,51717,628,344447,628,51717,344447,2],
>                   "+-++-+--")
>                , (1, [1,1], "+-")
>                , (990630,
>                   [833196,524568,361663,108056,28026,824639,269315,440977,440977,765458,
>                    988451,242440,948414,130873,773990,765458,130873,28026,853121,553636,
>                    581069,82254,735536,833196,898562,898562,940783,988451,540613,317306,
>                    623194,940783,571384,988451,108056,514374,97664],
>                   "--+---+-+++-+-+---++-++-+---+-+--+-++")
>                ]
