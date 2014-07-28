http://community.topcoder.com/stat?c=problem_statement&pm=9759

Problem Statement: UnknownNames

You are given a String[] questionMarkNames, where each element represents a
single name, and all names have the same length. Some of the letters are
missing, and those letters are represented by question marks ('?'). Arrange the
names in a vertical row, such that corresponding characters in each name are in
the same column. Then, order the names so that each column is sorted in
non-decreasing order from top to bottom. You can replace each question mark with
any letter to achieve this.

Return a String[] containing the lexicographically earliest ordering that you
can achieve. If there is no way to achieve the goal, return an empty String[]
instead. An ordering A comes before an ordering B if A contains an
alphabetically earlier name at the first index where they differ.

> main :: IO ()
> main = do
>   putStrLn "*** UnknownNames Solution ***"
