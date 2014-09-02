http://community.topcoder.com/stat?c=problem_statement&pm=3946

Many image editing programs have the ability to stitch two images together to
form one larger image. In this problem, you will be given two images,
represented by String[]'s. Each String[] represents a bitmap, where the ASCII
value of character j of element i represents the color of the pixel in row i,
column j of the image. Your task is to stitch the two images together.
Specifically, image A goes to the left of image B and overlap pixels from the
right of A and the left of B overlap. To prevent any obvious artifacts from the
stitching, you want to gradually blend the stitched region from image A to image
B. The ith pixel (indexed from 1) from the left in the overlapping region should
have a value of

((overlap+1-i)*a+(i*b))/(overlap+1)

where a and b are the values of the pixels from A and B, respectively.
Hence, the leftmost pixels in the overlapping region should have values of

(overlap*a+b)/(overlap+1).

In all cases, you should round the pixel values to the nearest integer,
rounding 0.5 up.'

> import Data.Char (ord, chr)

> -- | The 'overlapValue' function's provided index 'i' should be 1 based
> overlapValue :: Int -> Int -> Char -> Char -> Char
> overlapValue olap i a b =
>   let (a', b') = (ord a, ord b)
>       cval = fromIntegral ((olap+1-i)*a'+(i*b')) / fromIntegral (olap+1)
>   in chr $ round cval

> overlapString :: Int -> String -> String -> String
> overlapString olap left right =
>   map (\(i, a, b) -> overlapValue olap i a b) $ zip3 [1..] left right

> computeOverlaps :: Int -> String -> String -> Maybe String
> computeOverlaps olap left right =
>   let leftLen = length left
>       rightLen = length right
>       (lstr, lostr) = (take (leftLen - olap) left,
>                        drop (leftLen - olap) left)
>       (rostr, rstr) = (take olap right,
>                        drop olap right)
>       -- Overlapping string
>       ostr = overlapString olap lostr rostr
>   in if leftLen < olap || rightLen < olap
>      then Nothing
>      else Just (lstr ++ ostr ++ rstr)

> main :: IO ()
> main = do
>   putStrLn "*** Solution to WordFind ***"
>   answer

> answer = mapM_ (print . go) examples
>   where
>     go (olap, ls, rs) = map (uncurry $ computeOverlaps olap) $ zip ls rs
>     examples = [ (4, ["AAAAAA"], ["JJJJJ"])
>                , (4, ["14ABCD", "25EFGH", "36IJKL"],
>                      ["ABCD14", "EFGH25", "IJKL36"])
>                , (3, [" 32ygfd", "3uh53G:", ")O83gh3"],
>                      ["hsd$*(PH", "3G:$)(*P", "gh86$PBB"])
>                , (0, ["A"], ["A"])
>                ]
