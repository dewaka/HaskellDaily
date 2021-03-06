http://community.topcoder.com/stat?c=problem_statement&pm=1585

Problem Statement: BusinessTasks

A busy businessman has a number of equally important tasks which he must
accomplish. To decide which of the tasks to perform first, he performs the
following operation.

He writes down all his tasks in the form of a circular list, so the first task
is adjacent to the last task. He then thinks of a positive number. This number
is the random seed, which he calls n. Starting with the first task, he moves
clockwise (from element 1 in the list to element 2 in the list and so on),
counting from 1 to n. When his count reaches n, he removes that task from the
list and starts counting from the next available task. He repeats this procedure
until one task remains. It is this last task that he chooses to execute.

Given a String[] list representing the tasks and an int n, return the task which
the businessman chooses to execute.

> type Task = (String, Bool)
> type TaskList = [(Int, Task)]

> data TList = TList { tasks :: TaskList
>                    , nextTask :: Int
>                    } deriving (Show, Eq)

> updateTask :: Bool -> Int -> TaskList -> TaskList
> updateTask v n =
>   map $ \task@(t, (name, _)) -> if t==n then (t, (name, v)) else task

> pendingTasks :: TaskList -> TaskList
> pendingTasks = filter notDone
>   where
>       notDone (_, (_, done)) = not done

> pendingCount = length . pendingTasks

> setDoneCyclic :: Int -> TaskList -> TaskList
> setDoneCyclic num tl =
>   let pending = pendingTasks tl
>       pindex = (num - 1) `rem` (length pending)
>       (index, _) = pending !! pindex
>   in case pending of
>      [] -> tl
>      _ -> updateTask True index tl

> toTaskIndex :: [String] -> TaskList
> toTaskIndex xs = zip [1..] $ zip xs $ repeat False

> toTList :: [String] -> TList
> toTList xs = TList { tasks = ts, nextTask = 0 }
>   where
>     ts = zip [1..] $ zip xs $ repeat False

> cyclicDoTask :: Int -> TList -> TList
> cyclicDoTask nth k@(TList tl nt) =
>   let pending = pendingTasks tl
>       idxNext = length $ takeWhile (\(n, _) -> n /= nt) pending
>       pindex = (idxNext + nth - 1) `rem` (length pending)
>       pnextIdx = (pindex + 1) `rem` (length pending)
>       (index, _) = pending !! pindex
>       (nextTaskId, _) = pending !! pnextIdx
>   in case pending of
>     [] -> k
>     _ -> TList { tasks = updateTask True index tl, nextTask = nextTaskId }

> workTillLast n tl
>   | pendingCount tl <= 1 = tl
>   | otherwise = workTillLast n $ setDoneCyclic n tl

> workCyclicTillLast n tl
>   | pendingCount (tasks tl) <= 1 = tl
>   | otherwise  = workCyclicTillLast n $ cyclicDoTask n tl
>
> main :: IO ()
> main = do
>   putStrLn "*** Solution to BusinessTasks ***"
>   answer

> answer = mapM_ go examples
>   where
>     go (xs, n) = print
>                  $ map (fst . snd) $ pendingTasks $ tasks
>                  $ workCyclicTillLast n $ toTList xs
>     examples = [ (["a", "b", "c", "d"], 2)
>                , (["a","b","c","d","e"], 3)
>                , (["alpha","beta","gamma","delta","epsilon"], 1)
>                , (["a", "b"], 1000)
>                , (["a","b","c","d","e","f","g","h","i","j","k","l","m",
>                    "n","o","p","q","r","s","t", "u","v","w","x","y","z"], 17)
>                , (["zlqamum","yjsrpybmq","tjllfea","fxjqzznvg","nvhekxr","am",
>                    "skmazcey","piklp", "olcqvhg","dnpo","bhcfc","y","h","fj",
>                    "bjeoaxglt","oafduixsz","kmtbaxu", "qgcxjbfx","my","mlhy",
>                    "bt","bo","q"], 9000000)
>                ]
