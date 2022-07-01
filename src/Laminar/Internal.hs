{-# LANGUAGE OverloadedStrings #-}

module Laminar.Internal (taskList) where

import Control.Concurrent
import Laminar (Task (..))
import System.Random

-- Internal

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
