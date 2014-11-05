Reference paper: 'Functioal Pearl - Do we need Dependent Types'
Hint: Answer is No at least according to the functionality discussed on this paper.
This paper builds on similar techiniques as demostrated by Danvy for defining type safe
printfs within the Hindley-Milner type system.

We are going to use the following function to define generic
zipWith function which is applicable to arbitrary number of arguments
without specializing for each and every one of them like done in
the Haskell base libraries.

> import Prelude hiding (repeat, succ)

> (<<) :: [a -> b] -> [a] -> [b]
> (f:fs) << (x:xs) = f x : (fs << xs)
> _ << _ = []

> repeat :: a -> [a]
> repeat f = f : repeat f

In the general case
zipWithN f a1 a2 ... an === repeat f << a1 << a2 << ... << an

> ans = zipWith (+) [1..3] [3,2..1]
> ans2 = repeat (+) << [1..3] << [3,2..1] -- This is same as ans

> ans3 = zipWith3 (\x y z -> (x, y, z)) [1..3] ['a'..'c'] [8..11]
> ans4 = repeat (\x y z -> (x, y, z)) << [1..3] << ['a'..'c'] << [8..11] -- Same as ans3

When we use repeat and application (<<) technique to implement zipping operations
we are also providing the number of lists to be zipped.
Thus we need to supply the number of lists to be zipped in some fashion to a generic
function we are going to define.
For that we need to define numerals as follows.

> succ :: ([b] -> c) -> [a -> b] -> [a] -> c
> succ = \n fs as -> n (fs << as)

Now we can define numerals as follows using the succ function.

> zero = id
> one = succ zero
> two = succ one
> three = succ two

Now we can provide a generic N zipWith as follows.

> zipWithN :: ([a] -> t) -> a -> t
> zipWithN n f = n (repeat f)

Some examples now:

> zipWith2 = zipWith
> ans5 = zipWith2 (,) "hello" [1..5]
> ans6 = zipWithN two (,) "hello" [1..5]

This is such a nice technique which can be used not just in a non-strict language
like Haskell but also in strict ML family languages as well as authors note in
the paper.
