import Text.XML.HXT.Core
import Text.HandsomeSoup

main :: IO ()
main = do
  let doc = fromUrl "http://en.wikipedia.org/wiki/Narwhal"
  links <- runX $ doc >>> css "#body a"
  print links

fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

fib n = if n < 2
        then 0
        else fib (n-1) + fib (n-2)
