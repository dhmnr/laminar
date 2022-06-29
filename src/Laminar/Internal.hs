{-# LANGUAGE OverloadedStrings #-}

module Laminar.Internal (buildGraph, runTaskGraph, taskList) where

import Control.Concurrent
import Control.Exception (assert)
import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Random

-- types
data Task = Task
  { taskName :: Text,
    taskDeps :: [Text],
    taskAction :: IO ()
  }

data TaskGraph = TaskGraph
  { graph :: Map Text [Text],
    tasks :: [Task]
  }

-- Creating Taskgraphs

buildGraph :: [Task] -> TaskGraph
buildGraph ts = TaskGraph {graph = taskGraph, tasks = ts}
  where
    taskGraph = Map.fromList $ map (\x -> (taskName x, taskDeps x)) ts

taskByName :: TaskGraph -> Text -> Task
taskByName tg n = head $ filter (\x -> n == taskName x) (tasks tg)

-- Running TaskGraphs

runTaskGraph :: TaskGraph -> IO ()
runTaskGraph tg = do
  endSig <- newEmptyMVar
  runTaskGraph' tg endSig
  takeMVar endSig

runTaskGraph' :: TaskGraph -> MVar () -> IO ()
runTaskGraph' tg endSig = do
  stg <- newMVar etg
  runTaskName stg startTask
  where
    (etg, startTask) = addDefaultTasks tg endSig

-- Internal
type SharedTaskGraph = MVar TaskGraph

runTaskName :: SharedTaskGraph -> Text -> IO ()
runTaskName stg n = do
  tg <- readMVar stg
  runTask stg (taskByName tg n)

runTask :: SharedTaskGraph -> Task -> IO ()
runTask stg t = void . forkIO $ do
  updateTaskGraph stg (taskName t)
  taskAction t
  pollAndFork stg

updateTaskGraph :: SharedTaskGraph -> Text -> IO ()
updateTaskGraph stg t = modifyMVar_ stg update
  where
    update tg = return $ TaskGraph {graph = Map.map (delete t) (Map.delete t $ graph tg), tasks = tasks tg} -- use generic-lens

pollAndFork :: SharedTaskGraph -> IO ()
pollAndFork stg = do
  tg <- readMVar stg
  print (graph tg)
  let ts = Map.filter null (graph tg)
  mapM_ (runTask stg . taskByName tg) (Map.keys ts)

addDefaultTasks :: TaskGraph -> MVar () -> (TaskGraph, Text)
addDefaultTasks tg endSig = (tg'', sname)
  where
    tg'' = addEndTask tg' ename endSig
    tg' = addStartTask tg sname
    (sname, ename) = ("LaminarStart", "LaminarEnd")

-- getDefaultTaskNames :: TaskGraph -> (Text, Text)
-- getDefaultTaskNames = undefined

addStartTask :: TaskGraph -> Text -> TaskGraph
addStartTask (TaskGraph _ tl) name = buildGraph (startTask : tl)
  where
    startTask = Task {taskName = name, taskDeps = [], taskAction = putStrLn "Task LaminarStart Completed."}

addEndTask :: TaskGraph -> Text -> MVar () -> TaskGraph
addEndTask (TaskGraph _ tl) name endSig = buildGraph (endTask : tl)
  where
    endTask = Task {taskName = name, taskDeps = map taskName tl, taskAction = act}
    act = do
      putStrLn "Task LaminarEnd Completed."
      putMVar endSig ()

-- test data

taskList :: [Task]
taskList = [taskA, taskB, taskC, taskD, taskE]

taskA :: Task
taskA = Task "A" [] $ do
  randomDelay
  putStrLn "Task A Complete."

taskB :: Task
taskB = Task "B" ["A"] $ do
  randomDelay
  putStrLn "Task B Complete."

taskC :: Task
taskC = Task "C" ["A"] $ do
  threadDelay (10 ^ 7)
  putStrLn "Task C Complete."

taskD :: Task
taskD = Task "D" ["B"] $ do
  randomDelay
  putStrLn "Task D Complete."

taskE :: Task
taskE = Task "E" ["D", "C"] $ do
  randomDelay
  putStrLn "Task E Complete."

randomDelay :: IO ()
randomDelay = do
  randomRIO (500, 5000) >>= threadDelay
