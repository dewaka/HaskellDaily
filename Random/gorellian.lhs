http://www.reddit.com/r/dailyprogrammer/comments/20sjif/4192014_challenge_154_intermediate_gorellian/

The sort order of the alphabet is given.

We require sortBy from Data.List

> import Data.List (sortBy)

> type Alphabet = [(Char, Int)]

> posGChar :: Alphabet -> Char -> Int
> posGChar a c = case lookup c a of
>   Just n -> n
>   Nothing -> error $ "Invalid letter: " ++ (show c)

> compareGWord :: Alphabet -> String -> String -> Ordering
> compareGWord _ [] [] = EQ
> compareGWord _ [] _ = LT
> compareGWord _ _ [] = GT
> compareGWord a (x:xs) (y:ys) =
>   case (posGChar a x) `compare` (posGChar a y) of
>     EQ -> compareGWord a xs ys
>     GT -> GT
>     LT -> LT

> gsort :: Alphabet -> [String] -> [String]
> gsort a = sortBy (compareGWord a)

> exampleOrder :: String
> exampleOrder = "UVWXYZNOPQRSTHIJKLMABCDEFG"

> exampleAlphabet :: Alphabet
> exampleAlphabet = zip exampleOrder [0..]

> exampleWords :: [String]
> exampleWords = [ "ANTLER"
>                , "ANY"
>                , "COW"
>                , "HILL"
>                ,"HOW"
>                ,"HOWEVER"
>                ,"WHATEVER"
>                ,"ZONE"
>                ]

> main :: IO ()
> main = do
>   putStrLn "Here we go, doing Gorellian sorting"
>   putStrLn "Original order"
>   print exampleWords
>   putStrLn "Gorellian order"
>   print $ gsort exampleAlphabet exampleWords
