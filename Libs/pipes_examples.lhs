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

> doPipes = runEffect $ for stdinLn (lift . putStrLn)

> loop' :: Effect IO ()
> loop' = do
>   eof <- lift isEOF
>   unless eof $ do
>     str <- lift getLine
>     (lift . putStrLn) str
>     loop'

> loop :: Effect IO ()
> loop = for stdinLn $ \str -> do
>   lift $ putStrLn str

> main :: IO ()
> main = do
>   putStrLn "*** Pipes examples ***"
