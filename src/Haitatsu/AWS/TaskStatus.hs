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
import            Control.Monad.Writer
import qualified  Data.DList as D
import            Data.Foldable
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
      tell $ D.singleton ("    this is a dry run - pretending the service is healthy")
      pure $ Right (RunningCount 1)

    wet cluster service = taskStatus service selector
                      <$> describeService cluster service

describeService :: T.Text -> T.Text -> WetRun DescribeServicesResponse
describeService clusterName serviceName =
  sendAWS $ describeServices
          & ds1Services .~ [serviceName]
          & ds1Cluster .~ Just clusterName

type TaskStatus a = Either String a


findService :: T.Text
            -> DescribeServicesResponse
            -> TaskStatus ContainerService
findService service rsp =
    case filter isMatching (rsp ^. dsrServices) of
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

    when (desired /= running) $
      Left $ show desired <> " desired / " <>
             show running <> " running."

    when (length deployments > 1) $
      Left "Multiple deployments currently active."

    maybe (Left "No running count found!")
          (Right . RunningCount)
          running


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



