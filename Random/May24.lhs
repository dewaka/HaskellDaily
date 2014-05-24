-- http://www.reddit.com/r/dailyprogrammer/comments/236va2/4162014_challenge_158_intermediate_part_1_the/


This is the ASCII art problem from Reddit
         .
        ..
       ...
      ****
     *****
    ******
   -------
  --------
 +++++++++
++++++++++
abcdefghij

Simple rules of the ASCII art production
* if it's a letter then 'a'..'j' they are mapped to ASCII patterns shown above
  For example letter 'a' is mapped to '+'. Letter 'c' is mapped to '++-'.
  Mapped values should be printed vertically.
* if a letter is preceded by a number n (say 3a for example) then n number of vertical
  spaces should be printed before the pattern denoted by the letter

We need a simple mapping for letter -> patterns. To keep things very simple
an assoc list should be fine

We are going to require transpose from Data.List.

> import Data.List (transpose)

> letterPatterns :: [(Char, String)]
> letterPatterns = [ ('a', "+")
>                  , ('b', "++")
>                  , ('c', "++-")
>                  , ('d', "++--")
>                  , ('e', "++--*")
>                  , ('f', "++--**")
>                  , ('g', "++--***")
>                  , ('h', "++--***.")
>                  , ('i', "++--***..")
>                  , ('j', "++--***...")
>                  ]

Helper function to get a pattern for a letter

> pattern :: Char -> String
> pattern c =
>   case lookup c letterPatterns of
>     Just p -> p
>     Nothing -> error $ "Not a valid pattern letter: " ++ (show c)

> type Pat = (Char, Int)

> parsePatternString :: String -> [Pat]
> parsePatternString [] = []
> parsePatternString (n:p:xs)
>   | n >= '1' && n <= '9' = (p, read [n] :: Int) : parsePatternString xs
>   | otherwise = (n, 0) : parsePatternString (p:xs)
> parsePatternString [x] = [(x, 0)]

> patternString :: String -> [String]
> patternString ps = map pat $ parsePatternString ps
>   where
>     pat (c, n) = pattern c ++ (take n $ repeat ' ')

> homogenize :: [String] -> [String]
> homogenize xs = map (hgFunc mlen) xs
>   where
>     hgFunc len s = (take (len - length s) $ repeat ' ') ++ s
>     mlen = maximum $ map length xs

> patternString' :: String -> [String]
> patternString' = transpose . homogenize . map reverse . patternString

> printPatternString :: String -> IO ()
> printPatternString p = mapM_ putStrLn $ patternString' p

> main :: IO ()
> main = do
>   putStrLn "*** ASCII Art ***"
>   printPatternString "j3f3e3e3d3d3c3cee3c3c3d3d3e3e3f3fjij3f3f3e3e3d3d3c3cee3c3c3d3d3e3e3fj"
