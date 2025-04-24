{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage line =
    case words line of
        "I": ts : msg -> parseTsMsg ts msg Info
        "W" : ts : msg -> parseTsMsg ts msg Warning
        "E" : sev : ts : msg ->
            case reads sev :: [(Int, String)] of
                [(n, "")] -> parseTsMsg ts msg (Error n)
                _ -> Unknown line
        _ -> Unknown line
    where parseTsMsg ts msg mt = case reads ts :: [(Int, String)] of
                [(n, "")] -> LogMessage mt n (unwords msg)
                _ -> Unknown line

parse :: String -> [LogMessage]
parse file = parseHelper (lines file)
    where parseHelper msgs =
            case msgs of
                [] -> []
                (x:xs) -> parseMessage x : parseHelper xs

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert lm mt =
    case (lm, mt) of
        (Unknown _, msgTree) -> msgTree
        (msg, Leaf) -> msgNode msg
        (msg@(LogMessage _ currTs _), Node left nodeMsg@(LogMessage _ valTs _) right) ->
            if currTs <= valTs
                then
                    case left of
                        Leaf -> Node (msgNode msg) nodeMsg right
                        _ -> Node (insert msg left) nodeMsg right
                else
                    case right of
                        Leaf -> Node left nodeMsg (msgNode msg)
                        _ -> Node left nodeMsg (insert msg right)
        (_, _) -> error "Impossible"
    where msgNode msg = Node Leaf msg Leaf

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:lms) = insert lm (build lms)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMsgText . sortLogs . getRelevantErrs
    where sortLogs logs = inOrder (build logs)
          -- Get relevant error messages from logs
          getRelevantErrs [] = []
          getRelevantErrs (err@(LogMessage (Error sev) _ _):xs)
            | sev >= 50 = err : getRelevantErrs xs
            | otherwise = getRelevantErrs xs
          getRelevantErrs (_:xs) = getRelevantErrs xs
          -- Get text content of log message
          getMsgText [] = []
          getMsgText (LogMessage _ _ val:xs) = val : getMsgText xs
          getMsgText (_:xs) = getMsgText xs

-- Exercise 6
-- Stumped me!
