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

> updateAt xs i f = map update $ zip [0..] xs
>   where
>     update (n, x) = if n == i then f x else x

> connectNodes :: NodeMatrix -> Node -> Node -> NodeMatrix
> connectNodes nm@(NodeMatrix { connections = cm }) i j =
>   if i == j then nm
>   else nm { connections = cm'' }
>   where
>     cm' = updateAt cm i $ \ls -> updateAt ls j $ \_ -> 1
>     cm'' = updateAt cm' j $ \ls -> updateAt ls i $ \_ -> 1

> connectAvoiding ::  [Node] -> NodeMatrix -> Node -> Node -> (Bool, NodeMatrix)
> connectAvoiding blackList nm i j =
>   let nm' = connectNodes nm i j
>       nodes = connectedNodes nm' i
>   in if any (`elem` blackList) nodes
>      then (False, nm)
>      else (True, nm')

> connectMaxAvoiding :: [Node] -> NodeMatrix -> Node -> (NodeMatrix, Int)
> connectMaxAvoiding blackList nm i =
>   let slots = map fst
>               $ filter (\(j, p) -> j /= i && p /= 1)
>               $ zip [0..] $ connections nm !! i
>       updateMatrix (nm, count) slot =
>         case connectAvoiding blackList nm i slot of
>          (True, nm') -> (nm', count+1)
>          (False, nm') -> (nm', count)
>   in foldl updateMatrix (nm, 0) slots

> connectAllPossibleAvoiding :: [Node] -> NodeMatrix -> (NodeMatrix, Int)
> connectAllPossibleAvoiding blackList nm@(NodeMatrix { nodeCount = n }) =
>   foldl (\(m, c) i -> let (m', c') = connectMaxAvoiding blackList m i
>                       in (m', c+c')) (nm, 0) [0..n-1]

> example1 = parseNodeMatrix' ["000", "000", "000"]

> example4 = parseNodeMatrix' ["01000","10100","01010","00100","00000"]

> answer =
>   let xs = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
>       xs' = updateAt xs 0 $ \s ->
>                             updateAt s 0 (\_ -> 3)
>   in xs'

> main :: IO ()
> main = do
>   putStrLn "*** Solution to TheSwap"
>   print answer
