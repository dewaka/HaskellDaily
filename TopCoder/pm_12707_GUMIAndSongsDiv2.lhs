http://community.topcoder.com/stat?c=problem_statement&pm=12707

Problem Statement: GUMIAndSongsDiv2

Gumi loves singing. She knows N songs. The songs are numbered 0 through N-1.
She now has some time and she would love to sing as many different songs as
possible.

You are given a int[] duration. For each i, duration[i] is the duration of
song i in Gumi's time units.

Gumi finds it difficult to sing songs with quite different tones consecutively.
You are given a int[] tone with the following meaning: If Gumi wants to sing
song y immediately after song x, she will need to spend |tone[x]-tone[y]| units
of time resting between the two songs. (Here, || denotes absolute value.)

You are also given an int T. Gumi has T units of time for singing. She can start
singing any song she knows immediately at the beginning of this time interval.
Compute the maximal number of different songs she can sing completely within the
given time.

Examples:

0)
{3, 5, 4, 11}
{2, 1, 3, 1}
17
Returns: 3
There are four songs. Two songs have tone 1 and their durations are 5 and 11,
respectively. One song has tone 2 and its duration is 3. One song has tone 3 and
its duration is 4. Gumi has 17 units of time to sing.

It is impossible for Gumi to sing all four songs she knows within the given
time: even without the breaks the total length of all songs exceeds 17.

Here is one way how she can sing three songs: First, she sings song 0 in 3 units
of time. Second, she waits for |2-3|=1 unit of time and then sings song 2 in 4
units of time. Finally, she waits for |3-1|=2 units of time and then sings song
1 in 5 units of time. The total time spent is 3+1+4+2+5 = 15 units of time.

1)
{100, 200, 300}
{1, 2, 3}
10
Returns: 0
In this case, T is so small that she can't sing at all.

2)
{1, 2, 3, 4}
{1, 1, 1, 1}
100
Returns: 4
There is plenty of time, so she can sing all 4 songs.

3)
{10, 10, 10}
{58, 58, 58}
30
Returns: 3

4)
{8, 11, 7, 15, 9, 16, 7, 9}
{3, 8, 5, 4, 2, 7, 4, 1}
14
Returns: 1

5)
{5611,39996,20200,56574,81643,90131,33486,99568,48112,97168,5600,49145,73590,3979,94614}
{2916,53353,64924,86481,44803,61254,99393,5993,40781,2174,67458,74263,69710,40044,80853}
302606
Returns: 8

> import Data.List (sortBy)

> data Song = Song { duration :: !Int
>                  , tone :: !Int } deriving (Show, Eq)

> restBetween :: Song -> Song -> Int
> restBetween s1 s2 = abs $ tone s1 - tone s2

> totalRest (x:y:xs) = restBetween x y + totalRest (y:xs)
> totalRest _ = 0

> singDuration :: [Song] -> Int
> singDuration xs = sd + rd
>   where
>     sd = sum $ map duration xs
>     rd = totalRest xs

> compareSong :: Song -> Song -> Ordering
> compareSong s1 s2 = t1 `compare` t2
>   where
>     t1 = duration s1 + restBetween s1 s2
>     t2 = duration s2 + restBetween s1 s2

> orderSongs :: [Song] -> [Song]
> orderSongs = sortBy compareSong

> takeAsManySongs num xs =
>   let ys = orderSongs xs
>       go ts s = if singDuration (s:ts) <= num
>                 then (s:ts)
>                 else ts
>   in foldl go [] ys

> examples :: [([Int], [Int], Int)]
> examples = [ ([3, 5, 4, 11], [2, 1, 3, 1], 17)
>            , ([100, 200, 300], [1, 2, 3], 10)
>            , ([1, 2, 3, 4], [1, 1, 1, 1], 100)
>            , ([10, 10, 10], [58, 58, 58], 30)
>            , ([8, 11, 7, 15, 9, 16, 7, 9], [3, 8, 5, 4, 2, 7, 4, 1], 14)
>            , ([5611,39996,20200,56574,81643,90131,33486,
>                99568,48112,97168,5600,49145,73590,3979,94614],
>               [2916,53353,64924,86481,44803,61254,99393,
>                5993,40781,2174,67458,74263,69710,40044,80853], 302606)
>            ]

> answer = mapM_ (print . go) examples
>   where
>     go (ds, ts, num) =
>       let songs = map (uncurry Song) $ zip ds ts
>           songs' = orderSongs songs
>           ps = takeAsManySongs num songs'
>       in length ps

> main :: IO ()
> main = do
>   putStrLn "*** Solution to BuildingHeightsEasy ***"
>   answer
