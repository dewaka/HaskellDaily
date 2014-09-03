http://www.seas.upenn.edu/~cis194/spring13/lectures/08-IO.html

You are working on a compiler of sorts that really just translates the user's
code into another programming language which has its own compiler.
You need to implement code translation for method calls in your language.
The parameters passed into the method in your language will be given to you as a
String[], params. You will also be given a translation descriptor, code, which
tells you how to use the parameters in the code for the other language.
The translation descriptor will have "$X" where X is a number between 1 and the
number of parameters, inclusive, which signifies that the Xth parameter should
be inserted in at that spot (1-based).

For instance, if params is {"x", "y", "z"} and code is "($2 + $3)/$1",
you should return "(y + z)/x".

If there is ambiguity as to the number of the parameter, use the largest
possible valid number less than or equal to the number of parameters - $105
would be parameter 10 followed by the character '5' if there are at least 10
parameters but less than 105 and would be parameter 1 followed by "05" if there
are less than 10 parameters and at least one. Return code with all the
substitutions made.

> import Data.Char (isDigit)

> readMaxNum :: Int -> String -> Maybe (Int, String)
> readMaxNum bound xs =
>   let snum = takeWhile isDigit xs
>       go (rs, ts) s = let ts' = ts ++ [s]
>                           n :: Int
>                           n = read ts'
>                       in ((n, ts'):rs, ts')
>   in case snum of
>     [] -> Nothing
>     _ -> let (nums, _) = foldl go ([], []) snum
>              numCheck (num, snum) =
>                case snum of
>                  '0':_ -> True
>                  _ -> num > bound
>              maxTup = dropWhile numCheck nums
>          in case maxTup of
>            [] -> Nothing
>            (t:_) -> Just t

> paramSubstitute ps ('$':xs) =
>   case readMaxNum (length ps) xs of
>     Nothing -> '$' : paramSubstitute ps xs
>     Just (num, snum) -> ps !! (num-1) ++
>                         paramSubstitute ps (drop (length snum) xs)
> paramSubstitute ps (x:xs) = x : paramSubstitute ps xs
> paramSubstitute _ [] = []

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ParameterSubstitution ***"
>   answer

> answer = mapM_ (print . uncurry (flip paramSubstitute)) examples
>   where
>     examples = [ ( "if ($1 == $2) $3;"
>                  , ["12", "12", "doWhatIWant()"] )
>                , ( "$3+$1*$2-$7=$10"
>                  , ["myvar", "490jri", "e09jd9", "dlkjfoiej"] )
>                , ( "12342123$13231232$2123211242$a$dlkj$"
>                  , ["$2", "$1"] )
>                , ( "{[(+.*-,/\\:;<=>?@)]}_`~|$$1"
>                  , ["1{[(+.*-,/\\:;<=>?@)]}_`~|"] )
>                , ( "$01"
>                  , ["abc"] )
>                ]

Returns: "{[(+.*-,/\\:;<=>?@)]}_`~|$1{[(+.*-,/\\:;<=>?@)]}_`~|"
