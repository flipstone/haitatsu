module Haitatsu.AWS.Delivery
  ( deliver
  ) where

import            Haitatsu.AWS.HealthCheck
import            Haitatsu.AWS.InitialStatus
import            Haitatsu.AWS.RegisterTask
import            Haitatsu.AWS.Rollback
import            Haitatsu.AWS.UpdateService
import            Haitatsu.Types

deliver :: Haitatsu ()
deliver = do
  currentDeployment <- initialStatus
  taskRev <- registerTask

  doUpdate taskRev `onException` rollback currentDeployment

doUpdate :: TaskRevision -> Haitatsu ()
doUpdate taskRev = do
  updateService taskRev
  healthCheck taskRev

