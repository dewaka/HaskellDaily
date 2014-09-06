http://community.topcoder.com/stat?c=problem_statement&pm=8321

Problem Statement: RunningLetters

There is an electronic sign above the entrance to FIELD-Tech headquarters.
The sign displays a scrolling message that is repeated over and over again.
The letters show up on one side of the sign, scroll to the other side, and then
disappear. Polly, who works in the office, is curious about the length of the
message. She has observed the sign for some period of time and written down the
letters she has seen, in order. Now, you must help her determine the minimal
possible length of the message. For example, if she saw the letters "abcabcabcab",
two possible messages would be "bca" and "abcabc". The shortest possible length
would be 3.

You will be given a String[] running. Concatenate the elements of running to
get a space separated list of sections, each formatted "N S" (quotes for
clarity), representing the concatenation of N instances of S. Expand out all the
sections to get the entire text. For example, "3 abc 1 ab" expands out to
"abcabcabcab" (3 instances of "abc" followed by 1 instance of "ab").
Return the minimal possible message length that could have produced the given text.

> import Data.Char (isDigit, isSpace)
> import Data.List (tails, sortBy)

> expand xs n = concat $ take n $ repeat xs
> expansions = concat . map (uncurry expand)

> expandString xs = expansions ys
>   where
>     ys = go xs
>     go [] = []
>     go ls = (s, n) : go ls'
>       where
>         n = case takeWhile isDigit ls of
>              [] -> error "Illformatted expansion string"
>              ns -> read ns
>         sp = case dropWhile isDigit ls of
>               [] -> error "Illformatted expansion string"
>               rs -> dropWhile isSpace rs
>         s = case takeWhile (not . isSpace) sp of
>              [] -> error "Illformatted expansion string"
>              rs -> rs
>         ls' = case dropWhile (not . isSpace) sp of
>                [] -> []
>                rs -> dropWhile isSpace rs

> subs [] = [[]]
> subs xs@(_:xs') = inits xs ++ subs xs'
>   where
>     inits xs = [take n xs | n <- [1..length xs]]

> isPrefix [] _ = True
> isPrefix _ [] = False
> isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

> beforePattern _ [] = []
> beforePattern [] _ = []
> beforePattern pat xs@(x:xs')
>   | isPrefix pat xs = []
>   | otherwise = x : beforePattern pat xs'

> afterPattern _ [] = []
> afterPattern [] _ = []
> afterPattern pat xs
>   | isPrefix pat xs = afterPattern pat $ drop (length pat) xs
>   | otherwise = xs

> candidatePattern [] _ = False -- Trivial pattern is not a candidate pattern
> candidatePattern pat xs = isPrefix apat pat && isPrefix bpat' pat'
>   where
>     pat' = reverse pat
>     bpat' = reverse $ beforePattern pat xs
>     apat = afterPattern pat $ drop (length bpat') xs

> allCandidatePatterns xs = [cs | cs <- subs xs, candidatePattern cs xs]

> smallestCandidatePattern xs =
>   case dropWhile (not . flip candidatePattern xs) pats of
>    [] -> Nothing
>    (p:_) -> Just p
>   where
>     pats = sortBy ((. length) . compare . length) $ subs xs

> main :: IO ()
> main = do
>   putStrLn "*** Solution to RunningLetters ***"
>   answer

> answer = mapM_ go examples
>   where
>     go str = case smallestCandidatePattern $ expandString str  of
>               Nothing -> putStrLn "Error"
>               Just p -> print $ length p
>     examples = [ "3 abc 1 ab"
>                , "1 babaaba"
>                , "1 ba 1 c 1 bacba 3 cba"
>                , "42 runningletters 42 runningletters 1 running"
>                , "1 b "
>                ]
