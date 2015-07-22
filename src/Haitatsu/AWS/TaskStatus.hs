module Haitatsu.AWS.TaskStatus
  ( getTaskStatus
  , TaskSelector
  , primary
  , revision
  , TaskStatus(..)
  ) where

import            Control.Applicative
import            Control.Concurrent
import            Control.Lens
import            Control.Monad.Writer
import qualified  Data.DList as D
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Typeable
import            Network.AWS.ECS

import            Haitatsu.AWS.RegisterTask
import            Haitatsu.Types

getTaskStatus :: TaskSelector -> Haitatsu (TaskStatus ())
getTaskStatus selector =
    do echo Normal ("    checking status of task " <> selectorName selector)
       config <- asks environmentConfig
       status (serviceName config)

  where
    status service = Haitatsu (dry service) (wet service)

    dry service = do
      tell $ D.singleton ("    this is a dry run - pretending the service is healthy")
      pure $ Right ()

    wet service = taskStatus service selector <$> describeService service

describeService :: T.Text -> WetRun DescribeServicesResponse
describeService serviceName =
  sendAWS $ describeServices &
            ds1Services .~ [serviceName]

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

findDeployment :: TaskSelector
               -> ContainerService
               -> TaskStatus Deployment
findDeployment selector service =
    case filter (isSelected selector) (service ^. csDeployments) of
    [] -> Left "Deployment not found!"
    [d] -> Right d
    _ -> Left "Multiple Deployments found!"

deploymentStatus :: Deployment -> TaskStatus ()
deploymentStatus deployment =
    if desired == running
    then Right ()
    else Left msg
  where
    desired = deployment ^. dDesiredCount
    running = deployment ^. dRunningCount
    msg = show desired <> " desired / " <>
          show running <> " running."

taskStatus :: T.Text
           -> TaskSelector
           -> DescribeServicesResponse
           -> TaskStatus ()
taskStatus serviceName selector rsp = do
  service <- findService serviceName rsp
  deployment <- findDeployment selector service
  deploymentStatus deployment

newtype HealthCheckFailure = HealthCheckFailure String
  deriving (Show, Typeable)

instance Exception HealthCheckFailure



