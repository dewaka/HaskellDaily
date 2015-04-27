-- This is an interesting problem (and solution) I found today
-- http://blog.plover.com/prog/haskell/monad-search.html


--    S E N D
-- +  M O R E
--------------
--  M O N E Y
-- The problem is to find a S,E,N..Y <- [0..9] to get this relation

-- The solution discussed in the post is through the use of List Monad which is a beautiful sotlution

import Control.Monad (guard)
import Data.List ((\\), permutations)

digits :: [Int]
digits = [0..9]

toNumber :: [Int] -> Int
toNumber = foldl (\a b -> 10 * a + b) 0

solution = do
  s <- digits \\ [0]
  e <- digits \\ [s]
  n <- digits \\ [s,e]
  d <- digits \\ [s,e,n]
  let send = toNumber [s,e,n,d]
  m <- digits \\ [0,s,e,n,d]
  o <- digits \\ [s,e,n,d,m]
  r <- digits \\ [s,e,n,d,m,o]
  let more = toNumber [m,o,r,e]
  y <- digits \\ [s,e,n,d,m,o,r]
  let money = toNumber [m,o,n,e,y]
  guard $ send + more == money
  return (send, more, money)

-- Another way to do this would be
-- But this solution does twise as much work
-- returning solution' = [(9567,1085,10652),(9567,1085,10652)]
solution' = do
  [s,e,n,d,m,o,r,y,_,_] <- permutations [0..9]
  let send = toNumber [s,e,n,d]
      more = toNumber [m,o,r,e]
      money = toNumber [m,o,n,e,y]
  guard (s /= 0 && m /= 0 && send + more == money)
  return (send, more, money)
