import Data.Char (ord, chr)

-- |The function converts a Excel column to the corresponding decimal number
excelColumnToNum str = go (reverse str) 0
  where
    go [] _ = 0
    go (c:cs) n = d * (26 ^ n) + go cs (n+1)
      where
        d = ord c + 1 - ord 'A'
    
numToExcelColumn num = go num []
  where
    go n acc
      | n <= 0 = acc
      | otherwise = go (n - 27) (c:acc)
      where
        n' = n `rem` 27
        c = chr $ ord 'A' + n' - 1
