http://community.topcoder.com/stat?c=problem_statement&pm=10846

Problem Statement: NetworkXOneTimePad

One-time pad (patented by Vernam in 1919) is one of the most widely known schemes to encrypt
a binary string to achieve confidentiality. This scheme takes a binary string (a string consisting
of only the digits 0 and 1) as input and outputs another binary string of the same length.
The input is called the plaintext, and the output is called the ciphertext. The scheme uses a
key which is another binary string of the same length as the input. The i-th bit of the
ciphertext is defined as the XOR of the i-th bit of the plaintext and the key (see the notes
for XOR definition). The ciphertext is sent to the receiving party.

In this problem, we will consider several messages, each of length N, encrypted using a single
key of length N.

We would like to investigate how strong this cipher is. Suppose an adversary manages to find out
the content of all the original messages (i.e., the plaintexts) and some of the encrypted messages
(i.e., ciphertexts). These messages are given in the String[]s plaintexts and ciphertexts,
respectively. Return the number of possible keys that are consistent with this data.
The constraints will guarantee that there is at least one such key. A key is consistent if for all
members of ciphertexts C, there exists a member of plaintexts P such that when P is encrypted using
the specified key, it becomes C.

> import Data.Bits (xor)
> import Data.List (nub)
> import Data.Char (ord)

> xorKeys xs ys = [ map (uncurry xor) $ zip x y | x <- xs, y <- ys ]

> uniqueXORKeys xs ys = nub $ xorKeys xs ys

> digitToChar :: Int -> Char
> digitToChar n
>   | 0 <= n && n <= 9 = let c:_ = show n
>                        in c
>   | otherwise = error "Not a single digit"

> charToDigit :: Char -> Int
> charToDigit c
>   | ord '0' <= n && n <= ord '9' = n - ord '0'
>   | otherwise = error "Not a single digit character"
>   where
>     n = ord c

> toBitString :: [Int] -> String
> toBitString = map digitToChar

> fromBitString :: String -> [Int]
> fromBitString = map charToDigit

> main :: IO ()
> main = do
>   putStrLn "*** Solution to NetworkXOneTimePad ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go (xs, ys) = (length ans, ans)
>      where
>        ans = map toBitString $ uniqueXORKeys xs' ys'
>        xs' = map fromBitString xs
>        ys' = map fromBitString ys
>     examples = [ (["110", "001"], ["101", "010"])
>                , (["00", "01", "10", "11"], ["00", "01", "10", "11"])
>                , (["01", "10"], ["00"])
>                , (["000", "111", "010", "101", "110", "001"], ["011", "100"])
>                ]
