Post's correspondence problem as found in paper: 'When Maybe is not good enough'
URL: http://spivey.oriel.ox.ac.uk/wiki/images/0/06/Maybe.pdf

> type Tile = (String, String)

> correct :: [Tile] -> Bool
> correct layout = left == right
>   where
>     left = concat $ map fst layout
>     right = concat $ map snd layout

> choices :: Int -> [[Int]]
> choices n = concat (iterate step [[]])
>   where
>     step css = [ c : cs | c <- [0..n-1], cs <- css ]

> solutions :: [Tile] -> [[Int]]
> solutions tiles =
>   [ cs | cs <- tail (choices (length tiles)), correct (map (tiles!!) cs) ]

> tiles1 = [("b", "ca"), ("a", "ab"), ("ca", "a"), ("abc", "c")]
