http://community.topcoder.com/stat?c=problem_statement&pm=4670

Problem Statement: ListeningIn

You are creating an online multiplayer cooperative game. Players on a team
may chat with each other during the game, and you intend to take advantage
of this when building the AI to handle opponents. Part of the AI includes
determining whether a given phrase is part of a player's chat. Of course,
many variations of a given phrase are possible, and you want to detect as
many as you can. Shorthand is the most common example: instead of typing
'capture', a player might type 'cptr', or 'port to me' instead of
'teleport to me'.
You will be provided with a String typed typed by a player and a phrase
that you wish to check against. Return the characters removed from phrase to
obtain typed in the order they appear in phrase or "UNMATCHED" if there is no
way to obtain typed from phrase by simply removing characters. The constraints
ensure that the return is unique (there is only one option for which String
is returned).

First example given in the problem statement.
"cptr"
"capture"
Returns: "aue"

We need an auxiliary function which will take from a list until a given condition
is not satisfied, but differing from the Prelude takeWhile also returning the rest
of the list.

> takeWhile' p xs = go p xs []
>   where
>     go _ [] acc = (acc, [])
>     go p ls@(x:xs) acc
>       | p x = go p xs (acc++[x])
>       | otherwise = (acc, ls)

This will be like taking the disjoint of a set with the codition that
orders matter.

> removedItems sub str = go sub str []
>   where
>     go [] ys acc = Just $ acc ++ ys
>     go (x:xs) ys acc =
>       let (ps, ns) = takeWhile' (/=x) ys
>       in if null ns then Nothing
>          else let (h:ts) = ns
>               in if h==x then go xs ts (acc++ps)
>                  else Nothing

> listeningIn short full =
>   case removedItems short full of
>    Nothing -> "UNMATCHED"
>    Just s -> s

> example1 = listeningIn "cptr" "capture"
> example2 = listeningIn "port to me" "teleport to me"
> example3 = listeningIn "back  to the base" "back to the base"

> answer = mapM_ print [example1, example2, example3]

> main :: IO ()
> main = do
>   putStrLn "*** Solution for ListeningIn"
>   answer
