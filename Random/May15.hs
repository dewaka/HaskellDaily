-- This is some practice on point free form

-- Count elemnts equal to a given element in a list
countElem :: Eq a => a -> [a] -> Int
countElem e ls = length $ filter (==e) ls

countElem' :: Eq a => a -> [a] -> Int
countElem' e = length . filter (==e)

countElem'' :: Eq a => a -> [a] -> Int
countElem'' = (length .) . filter . (==)

countElem''' :: Eq a => [a] -> a -> Int
countElem''' = (length .) . flip (filter . (==))

-- Check if a list starts with a given element
startsWith :: Eq a => a -> [a] -> Bool
startsWith x ls = x == head ls

startsWith' :: Eq a => a -> [a] -> Bool
startsWith' x = (x ==) . head

startsWith'' :: Eq a => a -> [a] -> Bool
startsWith'' = (. head) . (==)

-- Sum all elements not equal to a given element
sumNotEqualTo :: (Eq a, Num a) => a -> [a] -> a
sumNotEqualTo x ls = sum $ filter (/=x) ls

sumNotEqualTo' :: (Eq a, Num a) => a -> [a] -> a
sumNotEqualTo' = (sum .) . filter . (/=)
