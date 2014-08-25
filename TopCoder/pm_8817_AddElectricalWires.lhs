http://community.topcoder.com/stat?c=problem_statement&pm=8817

You are given an electrical circuit for a home, with a number of nodes possibly
connected by wires. Any pair of nodes may be connected by at most one wire,
and a node can't be connected to itself. Each node on the circuit is either an
electrical outlet for the house or a connection to the main electrical grid.
The String[] wires tells you the wires that are already in place; the xth
character of the yth element is '1' (one) if nodes x and y have a wire between
them, '0' (zero) otherwise. The int[] gridConnections lists the indices of the
nodes that are connections to the main electrical grid.

You'd like to make the circuit safer and more redundant by adding as many extra
wires to it as possible. The one complication is that no two main grid
connections are currently wired together (directly or indirectly), and you must
preserve this, or else disaster will result. Determine the maximum number of
new wires you can add to the circuit.

*** Examples ***

{"000","000","000"}
{0}
Returns: 3
Every valid wire can be added.

{"000","000","000"}
{0,1}
Returns: 1
0 and 1 can't be connected, but 0 and 2 (or 1 and 2) still can be.

{"00000","00000","00000","00000","00000"}
{0,1,2,3,4}
Returns: 0
Any connections would be disastrous.

{"01000","10100","01010","00100","00000"}
{2,4}
Returns: 3

> import Data.List (nub)

> type Node = Int

> data NodeMatrix = NodeMatrix { connections :: [[Int]]
>                              , nodeCount :: Int
>                              } deriving (Show, Eq)

In this problem connections are non-directional. Thus if node 0 is
connected to node 2, then node 2 is also connected to node 0.
Thus they should both have respective positions in the matrix as 1.
This is one of the conditions we have to check in the given input as
to see whether it is a valid NodeMatrix in the first place.

> parseNodeMatrix :: [String] -> Maybe NodeMatrix
> parseNodeMatrix xs =
>   let cm = map (map toConn) xs
>       toConn '0' = 0
>       toConn '1' = 1
>   -- Todo: Improve error checking
>   in Just $ NodeMatrix cm $ length (cm !! 0)

> parseNodeMatrix' xs =
>   let Just nm = parseNodeMatrix xs
>   in nm

> directConnected :: NodeMatrix -> Node -> Node -> Bool
> directConnected NodeMatrix { connections = cm, nodeCount = n } i j =
>   if (0 <= i && i < n) && (0 <= j && j < n)
>   then (cm !! i) !! j == 1 && (cm !! j) !! i == 1
>   else False

> isConnected :: NodeMatrix -> Node -> Node -> Bool
> isConnected nm i j = j `elem` cnodes
>   where
>     cnodes = connectedNodes nm i

> connectedNodes :: NodeMatrix -> Node -> [Node]
> connectedNodes NodeMatrix { connections = cm } i =
>   filter (/=i) $ nub $ go i []
>   where
>     go i visited =
>       if i `elem` visited
>       then []
>       else let ds = map fst $ filter ((==1) . snd) $ zip [0..] (cm !! i)
>            in ds ++ concat [go d (i:visited) | d <- ds]

> example1 = parseNodeMatrix' ["000", "000", "000"]

> example4 = parseNodeMatrix' ["01000","10100","01010","00100","00000"]

> answer = undefined

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TheSwap"
>   answer
