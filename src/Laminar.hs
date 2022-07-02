{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Laminar
-- Copyright   :  (c) Dheemanth Manur 2022
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Dheemanth Manur <dheemanthmanur72@gmail.com>
-- Stability   :  experimental
--
-- This module provides a set of operations for running set of IO actions with
-- dependencies asynchronously.
-- The basic type is @'Task'@ which represents an IO action with a name and dependency list.
-- functions @'buildGraph'@ and @'runGraph'@ used to build and run @'TaskGraph'@ simultanouesly.
module Laminar (Task (..), TaskGraph (..), buildGraph, runGraph) where

import Control.Concurrent (MVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (Async, async, waitAny)
import Data.List (delete)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random (randomRIO)

-- | Container for all the details required for a Task
data Task = Task
  { -- | Text to uniquely identify a Task
    taskName :: Text,
    -- | List of dependent Task names
    taskDeps :: [Text],
    -- | IO Action that will be executed
    taskAction :: IO ()
  }

-- | Container for the dependency graph and task list
data TaskGraph = TaskGraph
  { -- | Dependency Map of every task to it's dependency list
    graph :: Map Text [Text],
    -- | List of tasks
    tasks :: [Task]
  }

-- Creating Taskgraphs

-- | Create a @'TaskGraph'@ from a list of @'Task'@
buildGraph :: [Task] -> TaskGraph
buildGraph ts = TaskGraph {graph = taskGraph, tasks = ts}
  where
    taskGraph = Map.fromList $ map (\x -> (taskName x, taskDeps x)) ts

taskByName :: TaskGraph -> Text -> Task
taskByName tg n = head $ filter (\x -> n == taskName x) (tasks tg)

-- Running TaskGraphs

-- | Execute a @'TaskGraph'@. Any Exceptions thrown by the tasks will be rethrown.
runGraph :: TaskGraph -> IO ()
runGraph tg = graphRunner tg []

graphRunner :: TaskGraph -> [Async Text] -> IO ()
graphRunner tg al =
  do
    let rt = runnable tg
    ar <- mapM (async . runTask . taskByName tg) rt
    let ual = al ++ ar
    if null ual
      then putStrLn "No more tasks to run."
      else do
        (t, tn) <- waitAny ual
        let utg = foldl updateGraph tg rt
        graphRunner utg (delete t ual)

runTask :: Task -> IO Text
runTask t = do
  taskAction t
  return $ taskName t

updateGraph :: TaskGraph -> Text -> TaskGraph
updateGraph (TaskGraph tg tl) tn = TaskGraph {graph = udg tg, tasks = tl}
  where
    udg = Map.map (delete tn) . Map.delete tn

runnable :: TaskGraph -> [Text]
runnable (TaskGraph tg _) = Map.keys $ Map.filter null tg

-- utils

putStrLn' :: MVar () -> String -> IO ()
putStrLn' mvar str = do
  putMVar mvar ()
  putStrLn str
  takeMVar mvar

-- test data

taskList :: [Task]
taskList = [taskA, taskB, taskC, taskD, taskE]

taskA :: Task
taskA = Task "A" [] $ do
  randomDelay
  putStrLn "Task A Complete."

taskB :: Task
taskB = Task "B" ["A"] $ do
  threadDelay $ 3 * (10 ^ 6)
  putStrLn "Task B Complete."

taskC :: Task
taskC = Task "C" ["A"] $ do
  threadDelay $ 1 * (10 ^ 6)
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
