http://community.topcoder.com/stat?c=problem_statement&pm=3520

Your company has just undergone some software upgrades, and you will now be
storing all of the names of your clients in a new database. Unfortunately,
your existing data is inconsistent, and cannot be imported as it is.
You have been tasked with writing a program to cleanse the data.

You are given a String[], names, which is a list of the names of all of your
existing clients. Some of the names are in "First Last" format, while the rest
are in "Last, First" format. You are to return a String[] with all of the names
in "First Last" format, sorted by last name, then by first name.

> import Data.List (sort)

> data Name = Name { firstName :: String
>                  , lastName :: String
>                  } deriving (Eq)

> instance Show Name where
>   show (Name f l) = f ++ " " ++ l

> -- Defines the comparitive order, thus sort order
> instance Ord Name where
>   (Name f1 l1) `compare` (Name f2 l2) = case l1 `compare` l2 of
>                                          EQ -> f1 `compare` f2
>                                          e -> e

> readName :: String -> Name
> readName name = if ',' `elem` name
>                 then readLegacyFormat
>                 else readNewFormat
>   where
>     readLegacyFormat =
>       let last = takeWhile (/=',') name
>           (',':' ':first) = dropWhile (/=',') name
>       in Name first last
>     readNewFormat =
>       let first = takeWhile (/=' ') name
>           (' ':last) = dropWhile (/=' ') name
>       in Name first last

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ClientsList ***"
>   answer

> answer = mapM_ go examples
>   where
>     go snames = let names = map readName snames
>                 in print $ sort names
>     examples = [ [ "Joe Smith", "Brown, Sam", "Miller, Judi" ]
>                , [ "Campbell, Phil", "John Campbell", "Young, Warren" ]
>                , [ "Kelly, Anthony", "Kelly Anthony", "Thompson, Jack" ]
>                , [ "Trevor Alvarez", "Jackson, Walter", "Mandi Stuart",
>                    "Martin, Michael", "Peters, Tammy", "Richard Belmont",
>                    "Carl Thomas", "Ashton, Roger", "Jamie Martin"]
>                , [ "Banks, Cody", "Cody Banks", "Tod Wilson" ]
>                , [ "Mill, Steve", "Miller, Jane" ]
>                ]
