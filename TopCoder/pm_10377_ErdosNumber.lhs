Problem Statement for ErdosNumber

The Erdos number is a way of describing the "collaborative distance" between a
scientist and Paul Erdos by authorship of scientific publications.

Paul Erdos is the only person who has an Erdos number equal to zero. To be
assigned a finite Erdos number, a scientist must publish a paper in
co-authorship with a scientist with a finite Erdos number. The Erdos number of a
scientist is the lowest Erdos number of his coauthors + 1. The order of
publications and numbers assignment doesn't matter, i.e., after each publication
the list of assigned numbers is updated accordingly.

You will be given a String[] publications, each element of which describes the
list of authors of a single publication and is formatted as
"AUTHOR_1 AUTHOR_2 ... AUTHOR_N" (quotes for clarity only). Paul Erdos will be
given as "ERDOS".

Return the list of Erdos numbers which will be assigned to the authors of the
listed publications. Each element of your return should be formatted as
"AUTHOR NUMBER" if AUTHOR can be assigned a finite Erdos number, and just
"AUTHOR" otherwise. The authors in your return must be ordered lexicographically.

Example:
When input is {"KLEITMAN LANDER", "ERDOS KLEITMAN"}
Returns: {"ERDOS 0", "KLEITMAN 1", "LANDER 2" }

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ErdosNumber Problem ***"
