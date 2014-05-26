http://www.reddit.com/r/dailyprogrammer/comments/26b42x/5232014_challenge_163_hard_intersecting_lines_in/

Descripton:
Given a typical x/y coordinate system we can plot lines. It would be interesting to know which lines intersect.

Input:
A series of lines from 1 to many to put in our 2-D space. The data will be in the form:
(label) (x1 y1) (x2 y2)
(label) will be a letter A-Z
(x1 y1) will be the coordinates of the starting point on line
(x2 y2) will be the coordinates of the ending point on line

example input:
A -2.5 .5 3.5 .5
B -2.23 99.99 -2.10 -56.23
C -1.23 99.99 -1.10 -56.23
D 100.1 1000.34 2000.23 2100.23
E 1.5 -1 1.5 1.0
F 2.0 2.0 3.0 2.0
G 2.5 .5 2.5 2.0
Max X can be 1,000,000,000.00
Max Y can be 1,000,000,000.00

Output:
The program will list which lines intersect. And which have 0 intersects.

Example Output:
Intersecting Lines:
A B
A C
A E
A G
F G
No intersections:
D

> data Line = Line { name :: Char
>                  , point1 :: (Float, Float)
>                  , point2 :: (Float, Float) } deriving (Show, Eq)

This is to find the m of y = mx + c general line equation

> inclination (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

This is to find the c of the general equation.
c = y - mx where m is the inclination

> constant p1@(x1, y1) p2@(x2, y2) = y1 - m*x1
>   where
>     m = inclination p1 p2

Now if two lines meet then they should have different inclinations. That is they
must not be parallel.
Then the next should be i = (c2-c1) / (m1-m2) is within the given range

> linesMeet line1 line2 = (m1 /= m2) && checkI && checkJ
>   where
>     m1 = inclination (point1 line1) (point2 line1)
>     m2 = inclination (point1 line2) (point2 line2)
>     c1 = constant (point1 line1) (point2 line1)
>     c2 = constant (point1 line2) (point2 line2)
>
>     intersectX = ((c2 - c1) / (m1 - m2))
>     intersectY = m1 * intersectX + c1
>     checkI = intersectX <= 1000000000.0
>     checkJ = intersectY <= 1000000000.0

Example lines

> exLineA = Line { name = 'A', point1 = (-2.5, 0.5), point2 = (3.5, 0.5) }
> exLineB = Line { name = 'B', point1 = (-2.23, 99.99), point2 = (-2.10, -56.23) }
> exLineC = Line { name = 'C', point1 = (-1.23, 99.99), point2 = (-1.10, -56.23) }
> exLineD = Line { name = 'D', point1 = (100.1, 1000.34), point2 = (2000.23, 2100.23) }
> exLineE = Line { name = 'E', point1 = (1.5, -1), point2 = (1.5, 1.0) }
> exLineF = Line { name = 'F', point1 = (2.0, 2.0), point2 = (3.0, 2.0) }
> exLineG = Line { name = 'G', point1 = (2.5, 0.5), point2 = (2.5, 2.0) }
