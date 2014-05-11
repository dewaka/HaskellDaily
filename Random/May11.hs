-- http://www.reddit.com/r/dailyprogrammer/comments/24ypno/572014_challenge_161_medium_appointing_workers/

import Data.List
import qualified Data.Map as M

data Worker = Worker { name :: !String } deriving (Show, Eq)

data Job = Job { job :: !String } deriving (Show, Eq, Ord)


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

workCandidates :: Ord b => [(t, [b])] -> M.Map b [t]
workCandidates = foldl workCandidate M.empty
  where
    workCandidate m (w, jobs) = foldl (updateJobsMap w) m jobs
    updateJobsMap w m j = M.alter (appendWorker w) j m
    appendWorker w Nothing = Just [w]
    appendWorker w (Just ws) = Just (w:ws)

-- Prints out job and possible workers in the ascending order of availability
-- This will give a good idea how to fill the buckets of job positions with
-- all available candidates without leaving anyone out
printJobsByAvailabilityOrder = do
  let candidatesList = M.toList $ workCandidates workerSkills
      candidateCount (_,as) (_,bs) = length as `compare` length bs
      orderedList = sortBy candidateCount candidatesList
  mapM_ print orderedList

allocate :: [(Job, Worker)]
allocate = undefined

-- How to find the solution
-- build a list of worker -> job list
-- then fill job number of buckets with workers. sort this list based on versatality
-- basis. that means david who can only do plumbing will be at the head

main :: IO ()
main = do
  putStrLn "Allocating workers"
