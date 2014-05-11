-- http://www.reddit.com/r/dailyprogrammer/comments/24ypno/572014_challenge_161_medium_appointing_workers/

data Worker = Worker { name :: !String } deriving (Show, Eq)

data Job = Job { job :: !String } deriving (Show, Eq)


-- Alice Wiring,Insulation,Plumbing
-- Bob Wiring,Decoration
-- Charlie Wiring,Plumbing
-- David Plumbing
-- Erin Insulation,Decoration,Finances

wiring = Job "Wiring"
insulation = Job "Insulation"
plumbing = Job "Plumbing"
decoration = Job "Decoration"
finances = Job "Finances"

jobsList = [ wiring
           , insulation
           , plumbing
           , decoration
           , finances
           ]

alice = Worker "Alice"
bob = Worker "Bob"
charlie = Worker "Charlie"
david = Worker "David"
erin = Worker "Erin"

workerSkills :: [(Worker, [Job])]
workerSkills = [ (alice, [wiring, insulation, plumbing])
               , (bob, [wiring, decoration])
               , (charlie, [wiring, plumbing])
               , (david, [plumbing])
               , (erin, [insulation, decoration, finances])]

allocate :: [(Job, Worker)]
allocate = undefined

main :: IO ()
main = do
  putStrLn "Allocating workers"
