-- http://community.topcoder.com/stat?c=problem_statement&pm=7869

import Data.List (nub)
import Data.Maybe (isJust, fromJust)

mirrorNumber :: Integer -> Maybe Integer
mirrorNumber num =
  case mirror [] $ show num of
    Nothing -> Nothing
    Just snum -> Just (read snum)
  where
    mirror acc [] = Just acc
    mirror acc ('5':xs) = mirror ('2':acc) xs
    mirror acc ('2':xs) = mirror ('5':acc) xs
    mirror acc ('8':xs) = mirror ('8':acc) xs
    mirror acc ('0':xs) = mirror ('0':acc) xs
    mirror acc ('1':xs) = mirror ('1':acc) xs
    mirror acc (_:xs) = Nothing

mirrorSameNum :: Integer -> Bool
mirrorSameNum num =
  case mirrorNumber num of
    Nothing -> False
    Just num' -> num == num'

answer m n =
  let mnums = filter mirrorSameNum [m..n]
  in length $ mnums

testInputs = [ (0, 10)
             , (0, 100)
             , (143, 23543) ]

main :: IO ()
main = do
  putStrLn "*** MirrorNumbers Solution ***"
  mapM_ (print . uncurry answer) testInputs
