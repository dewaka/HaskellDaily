Function to break a paragraph of text to given width where are words are justified

> justify :: String -> Int -> [String]
> justify line width = go line
>   where
>     go [] = []
>     go xs = let fs = take width xs
>                 rs = drop width xs
>             in case reverse fs of
>                 ' ':_ -> fs : go rs
>                 (x:rfs) -> case rs of
>                           [] -> [fs]
>                           ' ':_ -> fs : go rs
>                           _ -> (reverse rfs ++ "-") : go (x:rs)
