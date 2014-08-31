http://community.topcoder.com/stat?c=problem_statement&pm=6387

Problem Statement: XBallgame

Fans of the game X-ball frequently use the official players' stats.
These stats are presented as a list of players' names and the positions they can
play. If a player can play at several positions, he is shown in the list once
for each position.

The fans want to get the statistics in a new format. In the old format, each
element of the list contained a single player and a single position played by
that player. In the new format, each element of the list will contain a single
player and all the positions that the player can play. The new list must contain
the same number of elements as the old one, and players must appear in the same
order as before. Each element of the new list must contain the name of the
player, a '-' character, and a comma-separated list of all the positions the
player can play. The first position must be the same position that was in the
corresponding element of the old list, and the other positions must be sorted in
alphabetical order.

Old format stats:  |  New format stats:
John-DH            |  John-DH,RP,SP
Jack-SP            |  Jack-SP
Sam-OF             |  Sam-OF
John-SP            |  John-SP,DH,RP
John-RP

You will be given a String[] placeboard containing the statistics in the old
format. Each element will contain a player's name and an abbreviation of the
player's position in the form "Name-Position" (quotes for clarity).
Return the list after converting it to the new format.

> import Data.List (sort, nub)
> import Data.Maybe (isJust)

> type Name = String
> type Position = String
> type PlayerPosition = (Name, [Position])

> joinWith p ys = concat $ go ys
>   where
>     go (x:xs@(_:_)) = x:p : go xs
>     go e = e

> showPlayerPosition (player, positions) =
>   player ++ "-" ++ joinWith "," positions

> parseOldFormat :: String -> Maybe PlayerPosition
> parseOldFormat info =
>   let name = takeWhile (/='-') info
>       '-':position = dropWhile (/='-') info
>   in if null name || null position then Nothing
>      else Just (name, [position])

> aggregatePositions :: [PlayerPosition] -> Name -> PlayerPosition
> aggregatePositions pps name =
>   let positions = concatMap snd $ filter ((==name) . fst) pps
>   in (name, positions)

> convertFromOldFormat :: [String] -> [PlayerPosition]
> convertFromOldFormat xs =
>   let pps = reverse $ foldl go [] xs -- We need to reverse to keep same order
>       go ag x = case parseOldFormat x of
>                  Nothing -> ag
>                  Just p -> p:ag
>       display (player, [pos]) =
>         let otherPos = filter (/=pos) $ snd $ aggregatePositions pps player
>         in (player, pos:sort otherPos)
>   in fmap display pps

> displayInNewFormat xs =
>   mapM_ (print . showPlayerPosition) $ convertFromOldFormat xs

> main :: IO ()
> main = do
>   putStrLn "*** Solution to XBallGame ***"
>   answer

> answer = mapM_ go examples
>   where
>     go xs = do
>       displayInNewFormat xs
>       putStrLn ""
>     examples = [ ["John-DH", "Jack-SP", "Sam-OF", "John-SP", "John-RP"]
>                , ["John-DH", "Jack-SP", "Sam-OF", "John-SP", "John-RP",
>                   "John-RA", "John-RZ", "Jack-OF", "Sam-SP"]
>                , ["PFwL-GE", "PFwL-EV","agXL-SR", "PFwL-CS", "PFwL-FE",
>                   "PFwL-MS", "agXL-VS", "agXL-AL", "PFwL-BI", "PFwL-DK",
>                   "PFwL-MW", "PFwL-DS", "agXL-XJ", "agXL-PJ", "agXL-CK",
>                   "PFwL-KI", "agXL-KP", "agXL-EL", "PFwL-EA"]
>                , ["a-AA", "a-ZZ"]
>                ]
