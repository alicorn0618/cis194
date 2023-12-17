{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- 'I' for information messages
-- 'W' for warnings, and
-- 'E' for errors

-- Error message lines then have 1 integer indicating the severity of the error
-- with 1 being the sort of error you might get around to caring about some time

-- All types of log messages then have an integer time stamp followed by textual
-- content that runs to the end of the line

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : tsp : xs) -> LogMessage Info (read tsp :: Int) (unwords xs)
  ("W" : tsp : xs) -> LogMessage Warning (read tsp :: Int) (unwords xs)
  ("E" : sev : tsp : xs) -> LogMessage (Error (read sev :: Int)) (read tsp :: Int) (unwords xs)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ tsp _) (Node l lm@(LogMessage _ tsp' _) r) =
  if tsp < tsp' then Node (insert m l) lm r else Node l lm (insert m r)
insert _ _ = error "insert: unexpected pattern match"