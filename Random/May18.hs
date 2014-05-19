-- http://www.seas.upenn.edu/~cis194/hw/10-applicative.pdf

import Data.Char
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

-- using the above function
res0 = runParser (satisfy isUpper) "ABC"

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null xs = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Parser where
  fmap f (Parser p) = Parser f'
    where
      f' xs = case p xs of
        Nothing -> Nothing
        Just s -> Just $ first f s

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Parser where
  pure s = Parser f
    where
      f xs = Just (s, xs)

  (Parser f) <*> (Parser g) = Parser f'
    where
      f' xs = case g xs of
        Nothing -> Nothing
        Just (b, xs') -> case f xs' of
          Nothing -> Nothing
          Just (a, xs'') -> Just (a b, xs'')
