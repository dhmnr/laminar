module Laminar (Task (..), TaskGraph (..), buildGraph, runGraph) where

import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.Async
import Control.Monad (when)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Random (randomRIO)

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

runGraph :: TaskGraph -> IO ()
runGraph tg = graphRunner tg []

graphRunner :: TaskGraph -> [Async Text] -> IO ()
graphRunner tg al =
  do
    let rt = runnable tg
    -- print $ graph tg
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
