http://community.topcoder.com/stat?c=problem_statement&pm=4674

A substitution cipher is a simple method of encoding text in which all occurrences
of each letter are replaced by another. For example, consider the following
substitution table:

    original: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    encoded:  P X M S A C Z E V G I J D K N F O R L H T U W B Y Q
The message "REGISTER NOW FOR THE TOPCODER OPEN" would be encoded as
"RAZVLHAR KNW CNR HEA HNFMNSAR NFAK" by finding each letter in the top row and
replacing it with the letter directly beneath.

Substitution ciphers are relatively easy to break. One method takes advantage of
the fact that some letters are used more frequently than others. For example, in
English text, the letter 'E' occurs more often than any other. It is reasonable
to guess that the most common letter in the encoded text represents 'E'.

You will be given the encoded text as a String[] encoded. You will also be given
a String frequencyOrder containing all the letters that occur in the unencoded
text sorted in decreasing order by frequency. You are to attempt to decipher the
encoded text by replacing the most frequent letter in encoded with frequencyOrder[0],
the second-most frequent letter in encoded with frequencyOrder[1], and so on.
If some letters occur equally frequently in encoded, replace the letter that
occurs first alphabetically with the lower-index character in freqTableOrder.

Your method should return a String[] with exactly the same number of elements,
and characters in each element, as encoded. Replace only letters, preserving spaces.

> import Data.List (nub, sortBy)
> import Data.Char (isAlpha)

> updateAssoc d x _ [] = [(x, d)]
> updateAssoc d x f (p@(y, v):xs)
>   | x == y = (y, f v):xs
>   | otherwise = p : updateAssoc d x f xs

> freqTable xs = foldl updateTable [] xs
>   where
>     updateTable tb c = updateAssoc 1 c (+1) tb

> freqTable' init xs = foldl updateTable init xs
>   where
>     updateTable tb c = updateAssoc 1 c (+1) tb

> orderFreqTable tb = sortBy order tb
>   where
>     -- We are interested in the decreasing order for frequency values
>     -- but lexical order for characters in case of EQ in the first case
>     order (x, v1) (y, v2) =
>       case v2 `compare` v1 of -- value comparison case (dec)
>         EQ -> x `compare` y   -- alphabetic comparison case (asc)
>         e -> e

> computeFreqTable fs = foldl freqTable' [] $ map (filter isAlpha) fs

> encodingMap elist ftable = zip (map fst ftable) elist

> encodeString emap xs = map change xs
>   where
>     change x = case lookup x emap of
>                 Nothing -> x
>                 Just e -> e

We have to ignore spaces and other non-alphanumeric characters when computing the
frequency table.

> mapEncodedToFreq enc fs =
>   let oft = orderFreqTable $ freqTable enc'
>       enc' = filter isAlpha enc
>       ms = zip fs $ map fst oft
>       go xs (y, x) = map (\t -> if t==x then y else t) xs
>   in foldl go enc ms

> mapEncodedToFreq' fs enc =
>   let ftable = computeFreqTable fs
>       emap = encodingMap enc $ orderFreqTable ftable
>   in map (encodeString emap) fs

> answer = mapM_ (print . uncurry mapEncodedToFreq') examples
>   where
>     examples = [ (["ABBBCC"], "XYZ")
>                , (["RZLW"], "CEFD")
>                , (["XX YYYY Z YYY XX", "WWWWWZWWWWW"], "ETMQ")
>                , ([" X ", "", " ", "  ", "   "], "B")
>                , (["RAZVLHAR KNW CNR", "HEA HNFMNSAR NFAK"], "EORTPNFHSCDIWG")
>                ]

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TheSwap"
>   answer
