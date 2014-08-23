http://community.topcoder.com/stat?c=problem_statement&pm=3118

Problem Statement: HealthFood

Your favorite local restaurant has caught onto the health food trend, and has
begun publishing basic nutritional information for all of their most popular
menu selections. This is excellent timing, since your doctor has recently
recommended various diet plans for you and several of your friends.

You are given int[] protein, int[] carbs, and int[] fat, representing the
number of grams of protein, carbs, and fat in each of the available menu items.
Elements from each int[] correspond to the elements of the other int[]s with
the same index.

You are also given a String[] dietPlans, indicating the doctor's recommendation
for how each person should make their meal selection. Each element of dietPlans
describes the diet plan for an individual. Each character of each element of
dietPlans specifies, in order of importance, a selection criteria, defined in
the following way:

'C' = high carbs
'c' = low carbs
'P' = high protein
'p' = low protein
'F' = high fat
'f' = low fat
'T' = high calorie
't' = low calorie

As an example, the diet plan "tf" means the doctor recommends a meal with the
lowest possible calories, and if more than one is tied, the one with less fat
should be selected. Whenever more than one meal is tied according to the diet
plan, then the one with a lower index should be selected.

The restaurant sloppily neglected to list the total calorie count on the menu.
Fortunately, you happen to remember from days gone by that one gram of fat
contains 9 calories, and one gram of carbs or protein contains 5 calories.

You are to return a int[] indicating the indexes of the menu selections that
best suit each person's diet plan (indexed from 0). The return int[] should
have the same number of elements as dietPlans, and each value of the return
int[] should correspond to the element of dietPlans with the same index.

Example case:
{3, 4}
{2, 8}
{5, 2}
{"P", "p", "C", "c", "F", "f", "T", "t"}
Returns: { 1,  0,  1,  0,  0,  1,  1,  0 }
This is a simple menu, with only two selections. We see each of the simplest
diet plans here.

> import Data.List (sort, sortBy)

> data Diet = Diet { num :: Int
>                  , protein :: Int
>                  , carbs :: Int
>                  , fat :: Int } deriving (Show, Eq)

> calories :: Diet -> Int
> calories (Diet _ p c f) = 9*f + 5 * (c + p)

-- > toDiets = zipWith3 Diet

> zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
> zipWith4 _ _ _ _ _ = []

> toDiets = zipWith4 Diet

Dietary requirement should return a function which will actually order the
foods based on given conditions.

> dietCompareInc f d1 d2 = (f d2) `compare` (f d1)
> dietCompareDec f d1 d2 = (f d1) `compare` (f d2)

> dietOrdering [] = id
> dietOrdering ('P':xs) = sortBy (dietCompareInc protein) . dietOrdering xs
> dietOrdering ('p':xs) = sortBy (dietCompareDec protein) . dietOrdering xs
> dietOrdering ('C':xs) = sortBy (dietCompareInc carbs) . dietOrdering xs
> dietOrdering ('c':xs) = sortBy (dietCompareDec carbs) . dietOrdering xs
> dietOrdering ('F':xs) = sortBy (dietCompareInc fat) . dietOrdering xs
> dietOrdering ('f':xs) = sortBy (dietCompareDec fat) . dietOrdering xs
> dietOrdering ('T':xs) = sortBy (dietCompareInc calories) . dietOrdering xs
> dietOrdering ('t':xs) = sortBy (dietCompareDec calories) . dietOrdering xs

> solveOrdering ps cs fs ds =
>   let diets = toDiets [0..] ps cs fs
>       ordering x = let (d:_) = dietOrdering x diets
>                    in num d
>   in map ordering ds

First example is a very simple one.

> example1 =
>   solveOrdering [3, 4] [2, 8] [5, 2] ["P", "p", "C", "c", "F", "f", "T", "t"]

Second example.
{3, 4, 1, 5}
{2, 8, 5, 1}
{5, 2, 4, 4}
Ordering preferece list {"tFc", "tF", "Ftc"}
Solution should be: { 3,  2,  0 }

> example2 =
>   solveOrdering [3, 4, 1, 5] [2, 8, 5, 1] [5, 2, 4, 4] ["tFc", "tF", "Ftc"]

And indeed we get that right!

Lastly, the third example. The most complex one.
{18, 86, 76,  0, 34, 30, 95, 12, 21}
{26, 56,  3, 45, 88,  0, 10, 27, 53}
{93, 96, 13, 95, 98, 18, 59, 49, 86}
{"f", "Pt", "PT", "fT", "Cp", "C", "t", "", "cCp", "ttp", "PCFt", "P", "pCt", "cP", "Pc"}
Solution should be: { 2,  6,  6,  2,  4,  4,  5,  0,  5,  5,  6,  6,  3,  5,  6 }

> example3 =
>   let ps = [18, 86, 76,  0, 34, 30, 95, 12, 21]
>       cs = [26, 56,  3, 45, 88,  0, 10, 27, 53]
>       fs = [93, 96, 13, 95, 98, 18, 59, 49, 86]
>       ds = ["f", "Pt", "PT", "fT", "Cp", "C", "t", "",
>             "cCp", "ttp", "PCFt", "P", "pCt", "cP", "Pc"]
>   in solveOrdering ps cs fs ds

> answer =
>   mapM_ print [example1, example2, example3]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to HealthFood ***"
>   answer
