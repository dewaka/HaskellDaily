http://community.topcoder.com/stat?c=problem_statement&pm=2884

Problem Statement for Mailbox

We are going to sell a collection of individual plastic characters that customers
will buy and place on their mailboxes to spell out their addresses. We have a String[]
of "typical" addresses and want to be able to compare a proposed collection to these
addresses and determine how many are impossible to form from our collection.
We will count an address as impossible if it cannot be formed from our entire
collection of characters. Space characters in an address can always be formed
because the customer can form spaces by placing our plastic characters as he wishes.

Create a class Mailbox that contains a method impossible that is given a String
collection and a String[] address and that returns the number of elements in
address that are impossible to form from the given collection of characters.

Constraints
- collection will contain between 1 and 50 characters inclusive
- Each character in collection will be an uppercase letter 'A'-'Z' or a digit '0'-'9'.
- address will contain between 1 and 50 elements inclusive.
- Each element of address will contain between 1 and 50 characters inclusive.
- Each character in each element of address will be an uppercase letter 'A'-'Z'
  or a digit '0'-'9' or a space ' '.

Examples
0)
"AAAAAAABBCCCCCDDDEEE123456789"
{"123C","123A","123 ADA"}
Returns: 0
All these can be formed. The space in "123 ADA" can never be a problem, and
collection contains two 'A's.
We only consider one address at a time so it doesn't matter that we cannot form
both "123C" and "123A" at the same time from this collection.

1)
"ABCDEFGHIJKLMNOPRSTUVWXYZ1234567890"
{"2 FIRST ST"," 31 QUINCE ST", "606 BAKER"}
Returns: 3
We cannot form any of these. The first address requires two 'S's, the second
address requires a 'Q', and the third address requires two '6's.

2)
"ABCDAAST"
{"111 A ST", "A BAD ST", "B BAD ST"}
Returns: 2
"111 A ST" cannot be formed since collection does not contain any digits.
"B BAD ST" cannot be formed because it requires 2 'B's.

> import Data.List (nub)

> update d f x [] = [(x, d)]
> update d f x (t@(y, v):xs)
>   | x == y = (x, f v) : xs
>   | otherwise = t : update d f x xs

> freqTable :: (Eq a, Num n) => [a] -> [(a, n)]
> freqTable = foldl (flip $ update 0 (+1)) []

> canForm cs sub =
>   let cftable = freqTable cs
>       sftable = freqTable $ filter (/=' ') sub
>       canFind ctable (c, v) =
>         case lookup c ctable of
>           Nothing -> False
>           Just v' -> v' >= v
>   in all (canFind cftable) sftable

> main :: IO ()
> main = do
>   putStrLn "*** Solution to MailBox ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go (pool, cs) = let ps = filter id $ map (canForm pool) cs
>                     in length cs - length ps -- We are interested in cannot form count (hence length cs - length ps)
>     examples = [ ( "AAAAAAABBCCCCCDDDEEE123456789"
>                  , ["123C","123A","123 ADA"] )
>                , ( "ABCDEFGHIJKLMNOPRSTUVWXYZ1234567890"
>                  , ["2 FIRST ST"," 31 QUINCE ST", "606 BAKER"] )
>                , ( "ABCDAAST"
>                  , ["111 A ST", "A BAD ST", "B BAD ST"] )
>                ]
