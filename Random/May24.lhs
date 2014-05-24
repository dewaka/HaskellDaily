-- http://www.reddit.com/r/dailyprogrammer/comments/236va2/4162014_challenge_158_intermediate_part_1_the/


This is the ASCII art problem from Reddit
         .
        ..
       ...
      ****
     *****
    ******
   -------
  --------
 +++++++++
++++++++++
abcdefghij

Simple rules of the ASCII art production
* if it's a letter then 'a'..'j' they are mapped to ASCII patterns shown above
  For example letter 'a' is mapped to '+'. Letter 'c' is mapped to '++-'.
  Mapped values should be printed vertically.
* if a letter is preceded by a number n (say 3a for example) then n number of vertical
  spaces should be printed before the pattern denoted by the letter

We need a mapping

> main :: IO ()
> main = do
>   putStrLn "*** ASCII Art ***"
