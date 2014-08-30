http://community.topcoder.com/stat?c=problem_statement&pm=11965

Problem Statement: PointyWizardHats

The Order of All Things Pointy and Magical has commissioned the creation of some
new wizard hats. A wizard hat is created by taking two cones: a decorative top
cone, and a warm and fluffy bottom cone. To assemble the hat, both cones are
first placed onto a table, so that their bases are horizontal and their apexes
point upwards. The top cone is then lifted and placed onto the bottom cone.
The base of the top cone has to remain horizontal, and the apex of the top cone
must be strictly above the apex of the bottom cone.

Not every pair of cones can be used to create a wizard hat. A wizard hat is only
produced if the following two criteria are both met:
The apex of the top cone must be strictly above the apex of the bottom cone.
I.e., when the top cone is placed on top of the bottom cone and released, their
apexes must not touch.
Some part of the bottom cone must remain visible to form the brim of the hat.
(Otherwise, the hat would look like a simple cone, not like a wizard hat!)
You have several top cones and several bottom cones of various sizes.
Each cone can be described by its height (the distance between the apex and the
base) and by the radius of its base. The top cones you have are described by
topHeight and topRadius: for each valid i, you have one top cone with height
topHeight[i] and radius topRadius[i]. The bottom cones you have are described by
bottomHeight and bottomRadius in the same way.

Your task is to determine the maximum number of wizard hats you can make using
each of the available top and bottom cones at most once.

> import Data.List (sortBy)

> data Cone = Cone { height :: Int
>                  , radius :: Int
>                  } deriving (Show, Eq)

> type WizardHat = (Cone, Cone)

canMakeHat topHat bottomHat function returns whether we can use the topHat with
the bottomHat to make a Wizard hat. The condtion is that top one should be taller
than the bottom one and also that bottom one having a larger radius than the top
one.

> canMakeHat (Cone h1 r1) (Cone h2 r2) = (r2 > r1) && (h1 + r2 - r1 > h2)

> matchScore (Cone h1 r1) (Cone h2 r2) = abs (r1-r2)

> chooseFirst p ys = go ys []
>   where
>     go (x:xs) hs
>       | p x = (Just x, reverse hs ++ xs)
>       | otherwise = go xs (x:hs)
>     go [] hs = (Nothing, ys)

> chooseBestBottomCone :: Cone -> [Cone] -> (Maybe WizardHat, [Cone])
> chooseBestBottomCone top bs =
>   let ms = zipWith (\h k -> (matchScore top h, k)) bs bs
>       hatOrder (s1, _) (s2, _) = s1 `compare` s2
>       lmOrder = sortBy hatOrder ms
>   in case chooseFirst (canMakeHat top . snd) lmOrder of
>       (Nothing, rs) -> (Nothing, map snd rs)
>       (Just h, rs) -> (Just (top, snd h), map snd rs)

> allocateCones :: [Cone] -> [Cone] -> [WizardHat]
> allocateCones tops bottoms = wizHats
>   where
>     (wizHats, _) = foldl go ([], bottoms) tops
>     go (cs, bs) t = case chooseBestBottomCone t bs of
>       (Just wh, rs) -> (wh:cs, rs)
>       (Nothing, rs) -> (cs, rs)

> inputToHats :: [Int] -> [Int] -> [Cone]
> inputToHats = zipWith Cone

> main :: IO ()
> main = do
>   putStrLn "*** Solution to PointyWizardHats ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go ((h1s, r1s), (h2s, r2s)) =
>       let tops = inputToHats h1s r1s
>           bottoms = inputToHats h2s r2s
>           allocations = allocateCones tops bottoms
>       in (length allocations, allocations)
>     examples = [ ( ([30], [3]), ([3], [30]) )
>                , ( ([4, 4], [4, 3]), ([5, 12], [5, 4]) )
>                , ( ([3], [3]), ([1, 1], [2, 4]) )
>                , ( ([10, 10], [2, 5]), ([2, 9], [3, 6]) )
>                , ( ([1,2,3,4,5], [2,3,4,5,6]), ([2,3,4,5,6], [1,2,3,4,5]) )
>                , ( ([123,214,232,323,342,343], [123,123,232,123,323,434]), -- This case fails! Todo: Fix the matchScore algorithm
>                    ([545,322,123,545,777,999], [323,443,123,656,767,888]) )
>                , ( ([999,999,999,10000,10000,10000], [10000,10000,10000,1,2,3]),
>                    ([2324,2323,234,5454,323,232], [1,2,3222,434,5454,23]) )
>                ]

{999,999,999,10000,10000,10000}
{10000,10000,10000,1,2,3}
{2324,2323,234,5454,323,232}
{1,2,3222,434,5454,23}
