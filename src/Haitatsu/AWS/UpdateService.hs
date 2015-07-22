module Haitatsu.AWS.UpdateService where

import            Control.Lens
import            Control.Monad.Trans.AWS
import            Data.Monoid
import qualified  Data.Text as T
import            Network.AWS.ECS
import qualified  Network.AWS.ECS as ECS

import            Haitatsu.AWS.RegisterTask
import            Haitatsu.Types

updateService :: TaskRevision -> Haitatsu ()
updateService taskRev =
    do config <- asks environmentConfig
       let req = mkReq config

       echo Normal ("Updating service: " <> serviceName config)
       echo Normal ("    cluster: " <> clusterName config)
       echo Normal ("    desired count: " <> (T.pack $ show $ desiredCount config))
       echo Normal ("    task revision: " <> formatRevision taskRev)

       register req

       echo Normal "  = Done"
       echo Normal ""
  where
    mkReq :: EnvironmentConfig -> UpdateService
    mkReq config =
      ECS.updateService (serviceName config)
        & (usCluster .~ Just (clusterName config))
        & (usDesiredCount .~ Just (desiredCount config))
        & (usTaskDefinition .~ Just (formatRevision taskRev))

    register update = Haitatsu (dry update) (wet update)

    dry _ = pure ()
    wet req = sendAWS req >> pure ()

