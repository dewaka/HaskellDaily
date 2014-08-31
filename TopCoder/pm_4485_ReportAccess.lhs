http://community.topcoder.com/stat?c=problem_statement&pm=4485

Problem Statement: ReportAccess

You are working on a large database that holds information about several
different aspects of your company, such as customers, partners, prospects,
employees, products, etc. Every user in the system has access to one or
more types of data. For instance, a human resources worker might only have
access to employee records, while a sales person might have access to
prospects, customers, and products.

You are in charge of writing reports that summarize large volumes of this
information. However, since the reports may contain more than one type of data,
you need to be determine which users should be allowed to view that report.
In this case, assume that a user has permission to view a report if and only
if they have permission to view all of the types of data contained in that
report.

You are given a list of usernames in String[] userNames. You are given a list
of what data types each user can access in String[] allowedData, where each
element of allowedData is a space-delimited list of the permitted data types for
that user. Finally, you are given a String[] reportData, where each element of
report indicates a type of data included in the report.

You are to return a String[] indicating the user names of those users who should
be able to access the report. The list should be returned in alphabetical order.

Example:
{"joe", "nick", "ted"}
{"clients products", "products orders", "clients orders"}
{"clients", "products"}
Returns: {"joe" }
Here, only joe has access to both required data types.

> import Data.List (sort, words)

> intersection [] = []
> intersection (x:xs) = foldl go [] x
>   where
>     go acc t = if all (t `elem`) xs
>                then t:acc
>                else acc

> type ACL = (String, [String])

> usersWithAccess :: [ACL] -> String -> [String]
> usersWithAccess acls entity = sort $ foldl go [] acls
>   where
>     go acc (u, accessTo)
>       | entity `elem` accessTo = u:acc
>       | otherwise = acc

> usersWithMultiAccess :: [ACL] -> [String] -> [String]
> usersWithMultiAccess acls entities =
>   intersection $ map (usersWithAccess acls) entities

> inputToACL :: [String] -> [String] -> [ACL]
> inputToACL = zipWith $ \name entities -> (name, words entities)

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ReportAccess ***"
>   answer

> answer = mapM_ go examples
>   where
>     go (names, entities, check) = do
>       let acls = inputToACL names entities
>       print $ sort $ usersWithMultiAccess acls check
>     examples = [ (["joe", "nick", "ted"],
>                   ["clients products", "products orders", "clients orders"],
>                   ["clients", "products"])
>                , (["kathy", "john", "dan", "steve", "cheryl", "tony"],
>                   ["users data", "data orders", "users permissions",
>                    "system users controls", "default", "admin users"],
>                   ["users"])
>                , (["jim", "scott", "barbara"],
>                   ["users order products", "products shipping",
>                    "tracking products orders"],
>                   ["admin"])
>                ]


"kathy", "john", "dan", "steve", "cheryl", "tony"}
{"users data", "data orders", "users permissions", "system users controls", "default", "admin users"}
{"users"}
Returns: {"dan", "kathy", "steve", "tony" }

{"jim", "scott", "barbara"}
{"users order products", "products shipping", "tracking products orders"}
{"admin"}
