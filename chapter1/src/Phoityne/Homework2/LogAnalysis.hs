{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

createLogMessage :: MessageType -> String -> [String] -> LogMessage
createLogMessage mt t l = LogMessage mt (read t) (unwords l)

parseMessage :: String -> LogMessage
parseMessage s = case words s of 
    ("E":errType:timeStamp:message) -> createLogMessage (Error (read errType)) timeStamp message
    ("W":timeStamp:message)         -> createLogMessage Warning timeStamp message
    ("I":timeStamp:message)         -> createLogMessage Info timeStamp message
    list -> Unknown (unwords list)

parse :: String -> [LogMessage]
parse messages = case lines messages of
    (s:xs) -> (parseMessage s) : (parse (unlines xs))
    [] -> []
