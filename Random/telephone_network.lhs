http://www.reddit.com/r/dailyprogrammer/comments/25576s/592014_challenge_161_hard_phone_network/

Description:
Your company has built its own telephone network. This allows all your remote locations to talk to each other. It is your job to implement the program to establish calls between locations.
Calls are dedicated bandwidth on your network. It uses up resources on the network connection between locations. Because of this building a call between two locations on the network can be tricky. As a call is built it continues to use resources and new calls might have to route differently to find a way to reach the source and destination. If there are no ways to build a call then the call will fail.

Input:
There will be two sets of input. First set deals with what your phone network looks like. The second set will be the series of calls you must handle.

Network Input:
You must be able to read in network connections. They will be letter names for locations and a number. The number represents how many calls can go across the network link between these two locations. So for example if you have location A and location B and you can have 2 calls between these you will read in a link as:
A B 2
Example of list of links for a telephone network:
A B 2
A C 2
B C 2
B D 2
C E 1
D E 2
D G 1
E F 2
F G 2

Call Input:
You then have a list of calls to be placed on the network. Each call builds in the order you enter it and it is unknown if the resources will be there or not. You must read in all the calls. The calls simply have pairs listing the source and destination of the call. So for example if you wanted Location C to call Location G you would read in the call as:
C G
Example of calls to be placed on your example network:
A G
A G
C E
G D
D E
A B
A D

Output:
Your program will build the call if it can and list back the route the call took. If the call cannot be placed due to too many calls taking up resources it will indicate the "Call Failed".
Example output given the above inputs:
Call A G -- placed A B D G
Call A G -- placed A C E F G
Call C E -- placed C B D E
Call G D -- placed G F E D
Call D E -- failed
Call A B -- A B
Call A D -- failed

Understanding the Bandwidth:
So a link A B has a unit of "2" - if a call goes across this connection then the amount of calls the link can handle is reduced down to 1. If 1 more call crosses the link then the resource is 0 and the link is full. Any calls trying to be placed cannot cross this link as the bandwidth does not exist to support the call.
Links between locations can support calls in any direction. So a link A B exists the call can go A to B or B to A. In some cases you might have a call that is going over this link as A B and another call going B A.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Solution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            [A]
           |    |
          2        2
        |            |
      [B] ---- 2 ---- [C]

This is clearly a graph traversal problem with the added constraint that paths
has to be updated based on earlier connections - their weights have to be
decremented as we connect through them.

Pointers on representing graphs in Haskell
http://stackoverflow.com/questions/2785263/haskell-graph-data-type-representation

We are going to represent Node and its connections using the following data type.
connections - is a list of Nodes and weights

> type Name = Char

> data Node = Node { name :: Name
>                  , connections :: [(Name, Int)] } deriving (Show, Eq)

> type Graph = [Node]

> findNode :: Graph -> Name -> Maybe Node
> findNode [] _ = Nothing
> findNode (m@Node {name=n}:xs) p
>   | n == p = Just m
>   | otherwise = findNode xs p

> connectedNodes :: Graph -> Node -> Graph
> connectedNodes g n = foldl f [] $ (map fst . connections) n
>   where
>     f acc name = case findNode g name of
>       Nothing -> acc
>       Just node -> node:acc

> canConnect :: Graph -> Name -> Name -> Bool
> canConnect g a b = canConnect' g a b []
>   where
>     canConnect' g a b visited = if elem (a, b) visited
>                        then False
>                        else case findNode g a of
>                          Nothing -> False
>                          Just node -> if elem b $ map fst $ connections node
>                                       then True
>                                       else canSubConnect (map name $ connectedNodes g node) b ((a,b):visited)
> 
>     canSubConnect [] b visited = False
>     canSubConnect (x:xs) b visited = if canConnect' g x b visited
>                                      then True
>                                      else canSubConnect xs b visited
> 
>   

Sample setup to test

> nodeA = Node { name = 'A', connections = [ ('B', 2), ('C', 1) ] }
> nodeB = Node { name = 'B', connections = [ ('A', 2), ('C', 1), ('D', 3) ] }
> nodeC = Node { name = 'C', connections = [ ('A', 1), ('B', 1) ] }
> nodeD = Node { name = 'D', connections = [ ('B', 3) ] }
> nodeE = Node { name = 'E', connections = [] }

> graph = [nodeA, nodeB, nodeC, nodeD, nodeE]

> main :: IO ()
> main = do
>   putStrLn "Telephone network problem"
