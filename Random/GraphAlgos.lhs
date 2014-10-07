Some graph algorithm implementations from 'ML for the working programmer'

This is a very simple way to represent a graph.

> graph1 = [ ("a", "b"), ("a", "c"), ("a", "d")
>          , ("b", "e"), ("c", "f"), ("d", "e")
>          , ("e", "f"), ("e", "g") ]

Function to find all successors of a given node.

> nextNodes x = reverse . foldl (\ns (e, t) -> if e==x then t:ns else ns) []

Depth first search algorithm.

> depthFirst es g = reverse $ search es g []
>   where
>     search [] _ vs = vs
>     search (x:xs) g vs
>       | x `elem` vs = search xs g vs
>       | otherwise = search (nextNodes x g ++ xs) g (x:vs)

The following equivalent version is more efficient as it does not have (costly)
append operations.

> depthFirst' es g = reverse $ search es g []
>   where
>     search [] _ vs = vs
>     search (x:xs) g vs =
>       search xs g $ if x `elem` vs then vs
>                     else search (nextNodes x g) g (x:vs)

> main :: IO ()
> main = do
>   putStrLn "Hello Graph algorithms"
