http://community.topcoder.com/stat?c=problem_statement&pm=10862

Problem Statement: LotteryCheating

Bob likes to play the lottery, but it's so hard to win without cheating.
Each lottery ticket has an identifier which contains exactly n decimal digits.
If an identifier contains only '0's, it is considered a winning ticket.
Otherwise, the identifier is interpreted as a positive integer X written in
decimal notation (possibly with leading zeros). If X has an odd number of
positive integer divisors, it is a winning ticket, otherwise, it is not.
A positive integer is a divisor of X if it divides X evenly.

Unfortunately, Bob only has enough money to buy one ticket, and he cannot see
the identifier before buying a ticket. Therefore, he decides to cheat by buying
a ticket and modifying its identifier to make it a winning ticket. In a single
change operation, he can choose one digit, erase it, and print some other digit
in the same position. No other types of modifications are allowed. He can
perform any number of these change operations, but he wants to perform as few as
possible to minimize the risk of getting caught.

You are given a String ID, the initial identifier on Bob's ticket. Return the
minimal number of change operations necessary to transform the identifier into a
winning one. If the initial identifier is already a winner, return 0.

> divisors num = [n | n <- [1..num], num `rem` n == 0]

> isWinningNum num = num == 0 || odd (length $ divisors num)

This is an infinite series of all such winning numbers

> winningNums = filter isWinningNum [0..]

> changePos num p = [num' + i*n | i <- [0..9]]
>   where
>     n = 10^(p-1)
>     r = (num `rem` (n*10)) `div` n
>     num' = num - r*n

> onePosChanged num = concat $ map (changePos num) [1..length $ show num]
> twoPosChanged num = concat $ map onePosChanged $ onePosChanged num
> threePosChanged num = concat $ map onePosChanged $ twoPosChanged num
> fourPosChanged num = concat $ map onePosChanged $ threePosChanged num

TODO: Improve the following function to be able to handle generic cases

> minChanges num =
>   if isWinningNum num then 0
>   else if isWinningIn onePosChanged then 1
>        else if isWinningIn twoPosChanged then 2
>             else if isWinningIn threePosChanged then 3
>                  else if isWinningIn fourPosChanged then 3
>                       else -1
>   where
>     isWinningIn p = (not . null) $ filter isWinningNum $ p num

> examples = [1, 1234, 9000000000, 4294967296, 7654321]

> answer = mapM_ (print . minChanges) examples

> main :: IO ()
> main = do
>   putStrLn "*** Solution to LotteryCheating ***"
>   answer
