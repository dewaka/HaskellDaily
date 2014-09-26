http://community.topcoder.com/stat?c=problem_statement&pm=6010

Problem Statement: ProbabilisticTranslator

You want to translate a text from a source language to a target language.
The text is a sequence of words which you translate one by one using a dictionary,
replacing each word in the source text with one of its possible translations.
In order to find the most appropriate translation, you have explored a few books
in the target language, from which you have calculated expected frequency values
for all ordered pairs of consecutive words. To estimate the correctness of your
translation, you calculate the sum of the expected frequencies of all the
consecutive pairs of words in the translation. This sum is called the fidelity of the translation.

The source text is given as a String[], each element of which contains a non-empty
single-space separated list of words. The entire String[] should be considered a
single text, so the last word of an element is considered to be adjacent to the
first word of the next element.

The dictionary is given as a String[], each element of which is formatted as
"word : tr1 tr2 ... trn", where word is a word in the source language, and
"tr1 tr2 ... trn" is a single-space separated list of n possible translations of
that word in the target language (dots and quotes for clarity only).

The expected frequencies are given as a String[] containing elements of the form
"WORD1 WORD2 FREQUENCY" (quotes for clarity only), where FREQUENCY is the expected
frequency of WORD1 followed immediately by WORD2. If the frequency of some pair of
words is not given in the input, it is assumed to be zero.

Return the maximum possible fidelity of the translation.

Example1:
{"a b c"}
{"a : x y", "b : y z", "c : x z"}
{"y z 20", "x y 10", "z x 5"}
Returns: 30
The translation with the maximum possible fidelity is {"x y z"}.

> main :: IO ()
> main = do
>   putStrLn "*** Solution to ProbabilisticTranslator ***"
