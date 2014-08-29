> import Data.List (sort)

> songsInOrder limit songs =
>   take limit $ sort [show n ++ ".mp3" | n <- [1..songs]]

> answer = mapM_ (print . songsInOrder limit) examples
>   where
>     limit = 50
>     examples = [3, 10, 16, 32, 109]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to FoxAndMp3 ***"
>   answer
