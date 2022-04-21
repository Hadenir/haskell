{-# OPTIONS -Wall -Wextra #-}

module TaskManager where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Maybe

newtype PriorityQueue a = PriorityQueue [(Int, a)]
    deriving Show

insert :: PriorityQueue a -> (Int, a) -> PriorityQueue a
insert (PriorityQueue list) = PriorityQueue . insert' list
    where
        insert' :: [(Int, a)] -> (Int, a) -> [(Int, a)]
        insert' [] pair = [pair]
        insert' l@(pair'@(p', _):rest) pair@(p, _)
            | p >= p' = pair:l
            | otherwise = pair':insert' rest pair

front :: PriorityQueue a -> Maybe a
front (PriorityQueue list) = front' list
    where
        front' :: [(Int, a)] -> Maybe a
        front' [] = Nothing
        front' ((_,x):_) = Just x

pop :: PriorityQueue a -> PriorityQueue a
pop (PriorityQueue list) = PriorityQueue $ pop' list
    where
        pop' :: [(Int, a)] -> [(Int, a)]
        pop' [] = []
        pop' (_:xs) = xs

type Task = String
type StIO a = StateT (PriorityQueue Task) IO a

addTask :: Int -> Task -> StIO ()
addTask priority task = modify' (\pq -> insert pq (priority, task))

getTask :: StIO (Maybe Task)
getTask = do
    pq <- get
    put $ pop pq
    return $ front pq

ask :: String -> StIO String
ask prompt = lift (putStrLn prompt >> getLine)

addTaskCommand :: StIO ()
addTaskCommand = do
    prio <- ask "Priority?"
    task <- ask "Task?"
    addTask (read prio) task

getTaskCommand :: StIO ()
getTaskCommand = do
    task <- getTask
    lift $ putStrLn (fromMaybe "No more tasks" task)

processCommand :: String -> StIO Bool
processCommand "add" = addTaskCommand >> return True
processCommand "get" = getTaskCommand >> return True
processCommand "exit" = return False
processCommand _ = lift $ putStrLn "Unknown command" >> return True

readCommand :: StIO Bool
readCommand = lift getLine >>= processCommand

mainLoop :: StIO ()
mainLoop = do
    result <- readCommand
    when result mainLoop

main :: IO ()
main = do
    _ <- execStateT mainLoop $ PriorityQueue []
    putStrLn "Bye!"
