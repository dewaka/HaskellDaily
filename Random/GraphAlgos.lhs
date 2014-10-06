Some graph algorithm implementations from 'ML for the working programmer'

This is a very simple way to represent a graph.

> graph1 = [ ("a", "b"), ("a", "c"), ("a", "d")
>          , ("b", "e"), ("c", "f"), ("d", "e")
>          , ("e", "f"), ("e", "g") ]

Function to find all successors of a given node.

> nextNodes x = reverse . foldl (\ns (e, t) -> if e==x then t:ns else ns) []

Depth first search algorithm.

> depthFirst [] _ vs = reverse vs
> depthFirst (x:xs) g vs
>   | x `elem` vs = depthFirst xs g vs
>   | otherwise = depthFirst (nextNodes x g ++ xs) g (x:vs)

> main :: IO ()
> main = do
>   putStrLn "Hello Graph algorithms"
