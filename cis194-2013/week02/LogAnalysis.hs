{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Excercise 1
parseLogMessage :: MessageType -> String -> LogMessage
parseLogMessage t m = LogMessage t (parseMessageTimestamp m) (parseMessageText m)

parseMessageTimestamp :: String -> Int
parseMessageTimestamp s = toInt (head $ words s)

parseMessageText :: String -> String
parseMessageText m = unwords (drop 1 (words m))

parseSeverity :: String -> Int
parseSeverity s = toInt (head $ words s)

toInt :: String -> Int
toInt s = read s :: Int

parseMessage :: String -> LogMessage
parseMessage ('E':m) = parseLogMessage (Error (parseSeverity m)) (unwords (drop 1 (words m)))
parseMessage ('W':m) = parseLogMessage Warning m
parseMessage ('I':m) = parseLogMessage Info m
parseMessage s       = Unknown s

parse :: String -> [LogMessage]
parse m = map parseMessage (lines m)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t            -- unknown log message, don't change tree
insert lm Leaf = Node Leaf lm Leaf  -- message tree has no log message, add message to tree
insert lm (Node lc m rc)
    | logMessageTimestamp lm <= logMessageTimestamp m = Node (insert lm lc) m rc
    | otherwise                                       = Node lc m (insert lm rc)

errorTimestamp :: Int
errorTimestamp = -1
logMessageTimestamp :: LogMessage -> Int
logMessageTimestamp (LogMessage _ ts _) = ts
logMessageTimestamp (Unknown _)         = errorTimestamp

-- Exercise 3
build :: [LogMessage] -> MessageTree
build ms = insertMessages ms Leaf

insertMessages :: [LogMessage] -> MessageTree -> MessageTree
insertMessages [] mt = mt
insertMessages (m:ms) mt = insertMessages ms (insert m mt)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lc lm rc) = (inOrder lc) ++ [lm] ++ (inOrder rc)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = severErrorMessages (inOrder (build lms))

severErrorMessages :: [LogMessage] -> [String]
severErrorMessages [] = []
severErrorMessages (m:ms)
    | isSeverError m = (getMessageText m):(severErrorMessages ms)
    | otherwise      = severErrorMessages ms
    
isSeverError :: LogMessage -> Bool
isSeverError (LogMessage (Error sev) _ _)
    | sev >= 50 = True
    | otherwise = False
isSeverError _ = False

getMessageText :: LogMessage -> String
getMessageText (LogMessage _ _ m) = m
