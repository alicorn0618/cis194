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
