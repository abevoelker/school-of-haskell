module HW2.LogAnalysis
( parseMessage
, parse
, insert
, build
, inOrder
, whatWentWrong
) where

import Provided.Log

parseMessage :: String -> LogMessage
parseMessage x
    | flag == "I" = LogMessage Info timestamp message
    | flag == "W" = LogMessage Warning timestamp message
    where flag      = (words x)!!0
          timestamp = read ((words x)!!1) :: Int
          message   = unwords (drop 2 (words x))
parseMessage x
    | flag == "E" = LogMessage (Error errcode) timestamp message
    where flag      = (words x)!!0
          errcode   = read ((words x)!!1) :: Int
          timestamp = read ((words x)!!2) :: Int
          message   = unwords (drop 3 (words x))
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

build :: [LogMessage] -> MessageTree
build = undefined

inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
