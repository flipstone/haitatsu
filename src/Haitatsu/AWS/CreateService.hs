module Haitatsu.AWS.CreateService where

import            Control.Lens
import            Control.Monad.Trans.AWS
import            Data.Monoid
import qualified  Data.Text as T
import            Network.AWS.ECS
import qualified  Network.AWS.ECS as ECS

import            Haitatsu.AWS.RegisterTask
import            Haitatsu.Types

createService :: TaskRevision -> Haitatsu ()
createService taskRev =
    do config <- asks environmentConfig
       let req = mkReq config

       echo Normal ("Creating service: "  <> serviceName config)
       echo Normal ("    cluster: "       <> clusterName config)
       echo Normal ("    desired count: " <> (T.pack $ show $ desiredCount config))
       echo Normal ("    task revision: " <> formatRevision taskRev)
       echo Normal ("    load balancers: " <> (T.pack $ show $ loadBalancers config))

       register req

       echo Normal "  = Done"
       echo Normal ""
  where
    mkReq :: EnvironmentConfig -> CreateService
    mkReq config =
      ECS.createService (serviceName config)
        & (cs1Cluster .~ Just (clusterName config))
        & (cs1DesiredCount .~ Just (desiredCount config))
        & (cs1TaskDefinition .~ Just (formatRevision taskRev))

    register update = Haitatsu (dry update) (wet update)

    dry _ = pure ()
    wet req = sendAWS req >> pure ()

