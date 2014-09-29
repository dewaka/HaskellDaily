> import Pipes
> import Control.Monad (unless)
> import System.IO (isEOF)

> stdinLn :: Producer String IO ()
> stdinLn = do
>   eof <- lift isEOF
>   unless eof $ do
>     str <- lift getLine
>     yield str
>     stdinLn

> main :: IO ()
> main = do
>   putStrLn "*** Pipes examples ***"
