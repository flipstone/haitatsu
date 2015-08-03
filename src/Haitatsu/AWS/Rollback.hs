module Haitatsu.AWS.Rollback
  ( rollback
  ) where

import            Control.Lens
import            Control.Monad.Trans.AWS
import            Data.Monoid
import qualified  Data.Text as T
import            Network.AWS.ECS

import            Haitatsu.AWS.CurrentDeployment
import            Haitatsu.Types

rollback :: CurrentDeployment -> Haitatsu ()
rollback deployment =
    do config <- asks environmentConfig
       let rq = mkReq config

       echo Normal $ "Performing rollback"
       echo Normal $ "  = cluster: " <> (showT $ rq ^. usCluster)
       echo Normal $ "  = service: " <> (showT $ rq ^. usService)
       echo Normal $ "  = task   : " <> (showT $ rq ^. usTaskDefinition)
       echo Normal $ "  = desired: " <> (showT $ rq ^. usDesiredCount)

       update rq

  where
    mkReq :: EnvironmentConfig -> UpdateService
    mkReq config =
      updateService (serviceName config)
        & usCluster .~ Just (clusterName config)
        & usDesiredCount .~ Just (currentDesiredCount deployment)
        & usTaskDefinition .~ Just (currentTaskDefinition deployment)

    update rq = Haitatsu (dry rq) (wet rq)

    dry _ = dryEcho "    this is a dry run, not really rolling back"
    wet req = sendAWS req >> pure ()

    showT :: Show a => a -> T.Text
    showT = T.pack . show
