module Haitatsu.AWS.HealthCheck
  ( healthCheck
  ) where

import            Control.Concurrent
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Typeable

import            Haitatsu.AWS.RegisterTask
import            Haitatsu.AWS.TaskStatus
import            Haitatsu.Types

waitTimeSeconds :: Int
waitTimeSeconds = 30

bounceCheckTimeSeconds :: Int
bounceCheckTimeSeconds = 5

waitTimeMicroseconds :: Int
waitTimeMicroseconds = waitTimeSeconds *
                       1000 * -- milliseconds
                       1000   -- microseconds

healthCheck :: TaskRevision -> Haitatsu ()
healthCheck taskRev = do
  echo Normal ("Waiting for " <> formatRevision taskRev <> " to become healthy")
  waitForHealthy taskRev 0

waitForHealthy :: TaskRevision -> Int -> Haitatsu ()
waitForHealthy taskRev timeWaited = do
  config <- asks environmentConfig

  let timeRemaining = healthyWaitTimeSeconds config - timeWaited

  echo Normal ("  - " <> T.pack (show timeRemaining) <> " seconds remaining...")

  status <- getTaskStatus (revision taskRev)

  case status of
    Right (RunningCount 1) -> do
      echo Normal "  = Only 1 Task is running, starting bounce check."
      ensureSingleProcessNotBouncing taskRev 2

    Right _ -> do
      echo Normal "  = Task is healthy!"

    Left err -> do
      echo Normal ("    " <> T.pack err)

      if timeRemaining > 0
        then do wait waitTimeMicroseconds
                waitForHealthy taskRev (timeWaited + waitTimeSeconds)

        else throwM $ HealthCheckFailure $ "Task failed to become healthy within allotted time"

ensureSingleProcessNotBouncing :: TaskRevision -> Int -> Haitatsu ()
ensureSingleProcessNotBouncing taskRev checksRemaining
  | checksRemaining <= 0 =
    echo Normal "  = Everything appears to be healthy. It does not look like the task is bouncing."

  | otherwise = do
    echo Normal ("  = " <> T.pack (show checksRemaining) <> " bounce checks remaining.")
    status <- getTaskStatus (revision taskRev)

    case status of
      Right _ -> ensureSingleProcessNotBouncing taskRev (checksRemaining - 1)

      Left err -> do
        echo Normal ("    " <> T.pack err)
        throwM $ HealthCheckFailure $ "Singleton task is not healthy. It appears to be bouncing."

wait :: Int -> Haitatsu ()
wait micros = Haitatsu (pure ()) (liftIO $ threadDelay micros)

newtype HealthCheckFailure = HealthCheckFailure String
  deriving (Show, Typeable)

instance Exception HealthCheckFailure

