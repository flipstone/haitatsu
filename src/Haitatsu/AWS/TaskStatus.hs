module Haitatsu.AWS.TaskStatus
  ( getTaskStatus
  , TaskSelector
  , primary
  , revision
  , TaskStatus(..)
  , RunningCount(..)
  ) where

import            Control.Applicative
import            Control.Concurrent
import            Control.Lens
import            Data.Foldable
import            Data.Maybe
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Typeable
import            Network.AWS.ECS

import            Haitatsu.AWS.RegisterTask
import            Haitatsu.Types

newtype RunningCount = RunningCount Int
  deriving (Show, Eq)

getTaskStatus :: TaskSelector -> Haitatsu (TaskStatus RunningCount)
getTaskStatus selector =
    do echo Normal ("    checking status of task " <> selectorName selector)
       config <- asks environmentConfig
       status (clusterName config) (serviceName config)

  where
    status cluster service = Haitatsu (dry cluster service) (wet cluster service)

    dry _ _ = do
      dryEcho "    this is a dry run - pretending the service is healthy"
      pure $ Right (RunningCount 1)

    wet cluster service = taskStatus service selector
                      <$> describeService cluster service

describeService :: T.Text -> T.Text -> WetRun DescribeServicesResponse
describeService clusterName serviceName =
  sendAWS $ describeServices
          & dServices .~ [serviceName]
          & dCluster .~ Just clusterName

type TaskStatus a = Either String a


findService :: T.Text
            -> DescribeServicesResponse
            -> TaskStatus ContainerService
findService service rsp =
    case filter isMatching (rsp ^. dssrsServices) of
    [] -> Left "Service not found!"
    [s] -> Right s
    _ -> Left "Multiple services found!"
  where
    isMatching s = (s ^. csServiceName) == Just service

data TaskSelector = TaskSelector {
    isSelected :: Deployment -> Bool
  , selectorName :: T.Text
  }

revision :: TaskRevision -> TaskSelector
revision taskRev =
    TaskSelector isMatching taskName
  where
    isMatching d = isTaskDef (d ^. dTaskDefinition)

    isTaskDef Nothing = False
    isTaskDef (Just fullArn) =
      ("/" <> taskName) `T.isSuffixOf` fullArn

    taskName = formatRevision taskRev

primary :: TaskSelector
primary =
    TaskSelector isMatching "PRIMARY"
  where
    isMatching d = (d ^. dStatus) == Just "PRIMARY"

deploymentStatus :: TaskSelector -> [Deployment] -> TaskStatus RunningCount
deploymentStatus selector deployments =
  case find (isSelected selector) deployments of
  Nothing -> Left "Deployment not found!"
  Just deployment -> do
    let desired = deployment ^. dDesiredCount
        running = deployment ^. dRunningCount
        otherRunning = nonSelectedTasksRunning selector deployments

    when (desired /= running) $
      Left $ show desired <> " desired / " <>
             show running <> " running."

    when (otherRunning > 0) $
      Left $ "Waiting for " <>  show otherRunning <>
             " tasks from other revisions to stop."

    maybe (Left "No running count found!")
          (Right . RunningCount)
          running


nonSelectedTasksRunning :: TaskSelector -> [Deployment] -> Int
nonSelectedTasksRunning selector deployments =
    sum counts
  where
    notSelected = filter (not . isSelected selector) deployments
    counts = fromMaybe 0 . (^. dRunningCount) <$> notSelected

taskStatus :: T.Text
           -> TaskSelector
           -> DescribeServicesResponse
           -> TaskStatus RunningCount
taskStatus serviceName selector rsp = do
  service <- findService serviceName rsp
  deploymentStatus selector (service ^. csDeployments)

newtype HealthCheckFailure = HealthCheckFailure String
  deriving (Show, Typeable)

instance Exception HealthCheckFailure



