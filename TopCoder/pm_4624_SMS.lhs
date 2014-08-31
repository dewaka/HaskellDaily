http://community.topcoder.com/stat?c=problem_statement&pm=4624

Problem Statement: SMS

Short message service (SMS) has become a fast and quick method for communication.
Most service providers place a restriction on the size of messages and hence it
is important to write concise messages. Mobile phone users have found methods
for compressing their messages such that the content of the messages remains
unaltered. One such method is to take the original message and remove all
interior vowels from each word. A vowel is interior if there is at least one
consonant to the left and right (not necessarily adjacent) of the vowel in the
same word.

Given a String originalMessage with words separated by spaces return the
compressed version of the message.

> import Data.Char (toLower, isSpace)
> import Data.List (words)

> tokenize p xs = go xs [] []
>   where
>     go [] ps ns = (ps, ns)
>     go ys@(x:xs) ps ns
>       | p x = let s = takeWhile p ys
>                   xs' = dropWhile p ys
>               in go xs' (ps++[s]) ns
>       | otherwise = let n = takeWhile (not . p) ys
>                         xs' = dropWhile (not . p) ys
>                     in go xs' ps (ns++[n])

> tokenizeToWords :: String -> ([String], [String])
> tokenizeToWords = tokenize (not . isSpace)

> intersperse' (x:xs) (y:ys) = x ++ y ++ intersperse' xs ys
> intersperse' [x] [] = x       -- Commonly length xs = length ys + 1
> intersperse' _ _ = []

> vowel :: Char -> Bool
> vowel c = toLower c `elem` "aeiou"

> hasConsonants = any (not . vowel)

> smsCompress xs = foldl go [] $ zip xs [1..]
>   where
>     go acc (c, n)
>       | vowel c = if hasConsonants acc && hasConsonants (drop n xs)
>                   then acc
>                   else acc++[c]
>       | otherwise = acc++[c]

> smsCompressMessage :: String -> String
> smsCompressMessage msg =
>   let (ws, sps) = tokenizeToWords msg
>       sms = map smsCompress ws
>   in intersperse' sms sps

> main :: IO ()
> main = do
>   putStrLn "*** Solution to XBallGame ***"
>   answer

> answer = mapM_ go examples
>   where
>     go msg = do
>       putStrLn $ smsCompressMessage msg
>     examples = [ "Lets meet tomorrow"
>                , "Please come to my party"
>                , "I  like  your   style " -- Todo: Improve to ignore spaces
>                ]

> tests = [ smsCompress "Lets" == "Lts"
>         , smsCompress "Other" == "Othr"
>         , smsCompress "to" == "to"
>         , smsCompress "Oops" == "Oops"
>         , smsCompress "Loops" == "Lps"
>         ]
