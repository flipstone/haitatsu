module Haitatsu.AWS.CurrentDeployment
  ( getCurrentDeployment
  , CurrentDeployment(..)
  ) where

import            Control.Lens
import            Control.Monad.Trans.AWS
import            Data.Maybe
import            Data.Monoid
import qualified  Data.Text as T
import            Network.AWS.ECS

import            Haitatsu.Types

data CurrentDeployment = CurrentDeployment {
    currentTaskDefinition :: T.Text
  , currentDesiredCount :: Int
  , currentRunningCount :: Int
  }

getCurrentDeployment :: Haitatsu (Either T.Text CurrentDeployment)
getCurrentDeployment =
    do config <- asks environmentConfig

       echo Normal $ "Finding current deployment for "
                   <> serviceName config <> " @ "
                   <> clusterName config

       let rq = mkReq config

       result <- doIt rq

       case result of
         Right deployment -> do
           echo Normal  $ "  = Done"
           echo Verbose $ "    Task definition: " <> currentTaskDefinition deployment
           echo Verbose $ "    Running count  : " <> showT (currentRunningCount deployment)
           echo Verbose $ "    Desired count  : " <> showT (currentDesiredCount deployment)
           echo Normal  $ ""

         Left err -> do
           echo Normal $ "  = Error: " <> err
           echo Normal $ ""


       pure result
  where
    mkReq :: EnvironmentConfig -> DescribeServices
    mkReq config =
      describeServices
        & dServices .~ [serviceName config]
        & dCluster .~ Just (clusterName config)

    doIt update = Haitatsu (dry update) (wet update)

    dry _ = do
      dryEcho "    this is a dry run - making up a deployment"
      pure $ Right $ CurrentDeployment {
          currentTaskDefinition = "dryrun-deployment:fake"
        , currentDesiredCount = 2
        , currentRunningCount = 2
        }

    wet :: DescribeServices -> WetRun (Either T.Text CurrentDeployment)
    wet req = responseCurrentDeployment <$> sendAWS req

    showT = T.pack . show

responseCurrentDeployment :: DescribeServicesResponse -> Either T.Text CurrentDeployment
responseCurrentDeployment response =
  case response ^. dssrsServices of
  [] -> Left "Service not found!"
  [s] -> serviceCurrentDeployment s
  _ -> Left "Multiple services found!"

serviceCurrentDeployment :: ContainerService -> Either T.Text CurrentDeployment
serviceCurrentDeployment service =
  case service ^. csDeployments of
  [] -> Left "No deployments found!"
  [d] -> do
    when (d ^. dStatus /= Just "PRIMARY") $
      Left $ "Current deployment is not PRIMARY, its status was: "
          <> fromMaybe "<unknown>" (d ^. dStatus)

    taskDef <- maybe (Left "No task definition present on deployment!")
                     pure
                     (d ^. dTaskDefinition)

    running <- maybe (Left "No running count present on deployment!")
                     pure
                     (d ^. dRunningCount)

    desired <- maybe (Left "No desired count present on deployment!")
                     pure
                     (d ^. dDesiredCount)

    pure $ CurrentDeployment {
        currentTaskDefinition = taskDef
      , currentRunningCount = running
      , currentDesiredCount = desired
      }

  _ -> Left "Multiple deployments found!"

