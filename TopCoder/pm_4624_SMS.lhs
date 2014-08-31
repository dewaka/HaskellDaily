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

> import Data.Char (toLower)
> import Data.List (words)

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

> smsCompressMessage msg =
>   let ws = words msg
>   in map smsCompress ws

> main :: IO ()
> main = do
>   putStrLn "*** Solution to XBallGame ***"
>   answer

> answer = mapM_ (go . smsCompressMessage) examples
>   where
>     go cs = do
>       mapM_ (\s -> do
>                 putStr s
>                 putStr " "
>             ) cs
>       putStrLn ""
>     examples = [ "Lets meet tomorrow"
>                , "Please come to my party"
>                , "I  like  your   style" -- Todo: Improve to ignore spaces
>                ]

> tests = [ smsCompress "Lets" == "Lts"
>         , smsCompress "Other" == "Othr"
>         , smsCompress "to" == "to"
>         , smsCompress "Oops" == "Oops"
>         , smsCompress "Loops" == "Lps"
>         ]
