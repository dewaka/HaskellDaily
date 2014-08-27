This is an interview question I asked today.
To shift an array's 0 elements to the end of it. In Haskell instead of an
array I will be using ubiquitous lists.

> shiftZeros xs = xs' ++ zeros
>   where
>     xs' = filter (/= 0) xs
>     zeros = take (length xs - length xs') $ repeat 0
