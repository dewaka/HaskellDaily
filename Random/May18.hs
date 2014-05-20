-- http://www.seas.upenn.edu/~cis194/hw/10-applicative.pdf

import Data.Char
import Control.Applicative
import Control.Monad

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
      f' xs = case f xs of
        Nothing -> Nothing
        Just (h, xs') -> case g xs' of
          Nothing -> Nothing
          Just (a, xs'') -> Just (h a, xs'')

-- First Applicative law
-- f <$> x = pure f <*> x

-- Now to check whether my Applicative definition for Parser
-- is correct
res1 = runParser ((*10) <$> posInt) "78hello" -- Just (780, "hello)
res2 = runParser (pure (*10) <*> posInt) "78hello" -- Should be same!
check1 = res1 == res2

parser0 :: Parser (Char, Char)
parser0 = Parser f
  where
    f ('a':'b':xs) = Just (('a', 'b'), xs)
    f _ = Nothing

-- Now how to write the same Parser without using Parser constructor
parser1 :: Parser (String -> String)
parser1 = pure id

fun1 ('a':'b':xs) = Just (('a', 'b'), xs)
fun1 _ = Nothing

parser2 :: Parser (String -> Maybe ((Char, Char), String))
parser2 = pure fun1
