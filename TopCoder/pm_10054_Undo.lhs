http://community.topcoder.com/stat?c=problem_statement&pm=10054

Problem Statement: Undo

You are writing a simple text editor that supports only the following two commands:

"type c" where c is a character: Append character c to the end of the current text.
"undo t" where t is an integer: Undo all operations that were performed in the
previous t seconds in reverse order.
All quotes are for clarity only. The text in the editor is initially empty.

For example, consider the following sequence of commands:

Second 1: type a
Second 2: type b
Second 3: type c
Second 5: undo 3
At the end of second 3, the text is "abc". At second 5, all commands performed
in the previous 3 seconds are undone in reverse order. This means 'c' is
removed, and then 'b' is removed. The text becomes just "a".

Note that "undo" commands can also be undone. For example:

Second 1: type a
Second 2: type b
Second 3: undo 2
Second 4: undo 2
After second 2, the text is "ab". After second 3, everything is undone, and the
text becomes empty. At second 4, the previous "undo" is undone, so the text becomes
"ab" again. Then, the "type b" is also undone and the text becomes just "a".

You are given a String[] commands and a int[] time. Each element of commands is
a single command, and commands[i] is performed at time[i]. The commands are
given in chronological order. Return the text after all the commands are executed.

> main :: IO ()
> main = do
>   putStrLn "*** Solution for Undo ***"
