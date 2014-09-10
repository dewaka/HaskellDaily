http://community.topcoder.com/stat?c=problem_statement&pm=3094

Steganography is a method of cryptography where a message or entire document can
be hidden inside of another file or image which shows no evidence that there is
data hidden in it. Typically, the message or document to be sent is first
encrypted and compressed, and then combined with an existing file in the bits
that are less significant.

For example, the encrypted, compressed file could be combined with a bitmap image,
and each byte of the file could be encrypted into the lowest two bits of 4 pixel
values. The result would be that there is a 1/4 chance that a given pixel is
completely unchanged, and a 3/4 chance that it is changed, but by such a small
amount that it is essentially undetectable.

While it is not as secure or efficient, you could quite easily encrypt a message
into a bitmap image with no encryption or compression, and your message would be
protected by the fact that the image has no distinguishable traits that suggest
that a message is hidden in it. You will be given an "image" and you will encode
a given message into it and return the new image. The returned image should be
in the same format as the original image.

The image will be in the format of a String[] where each three digits represent
a number from 0 to 255, inclusive (leading zeros will be added as necessary),
which is a pixel value in the image. You will also be given a String, message
which contains the message you would like to encode into the image. You will
first encode the message into numbers representing the characters in the message -
spaces will be 0, 'A'-'Z' will be 1-26, 'a'-'z' will be 27-52, '0'-'9' will be
53-62, and 63 will be used for any space after the message.
All these numbers can be represented in binary with 6 digits. You will put each
pair of bits (representing a number between 0 and 3) into the lowest two bits of
the values in the image. For each character, you will put in the lowest two bits,
then the middle two, then the highest two, and then continue to the next character.
You will put them in the lowest two bits of the first pixel on the first row, then
the second pixel on the first row, and so on until you get to the end of the first
row, then the first pixel on the second row, and so on. Once you are out of
characters, continue substituting the lowest two bits of each pixel value as if
the current character were represented by number 63.

> import Data.Char (ord, chr)
> import Data.Bits ((.&.), xor, shift)

> encodeChar :: Char -> Int
> encodeChar c
>   | c == ' ' = 0
>   | 'A' <= c && c <= 'Z' = 1 + ord c - ord 'A'
>   | 'a' <= c && c <= 'z' = 27 + ord c - ord 'a'
>   | '0' <= c && c <= '9' = 53 + ord c - ord '0'
>   | otherwise = error "Encode char out of valid range"

> endMsgValue = 63

> encodeMessage = map encodeChar

> toBinary num = reverse $ go num
>   where
>     go 0 = []
>     go n = n .&. 1 : go (n `shift` (-1))

> splitImage :: [String] -> [[Int]]
> splitImage lines = map (map read . go) lines
>   where
>     go [] = []
>     go xs = take 3 xs : go (drop 3 xs)

> main :: IO ()
> main = do
>   putStrLn "*** Solution for ImageSteganography ***"
>   answer

> answer = undefined
