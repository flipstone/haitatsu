module Haitatsu.Command
  ( Command(..)
  , runCommand
  , AWS.TaskRevision(..)
  ) where

import qualified  Haitatsu.AWS as AWS
import            Haitatsu.Types

data Command =
    Deliver
  | RegisterTask
  | CreateService AWS.TaskRevision
  | UpdateService AWS.TaskRevision
  | HealthCheck AWS.TaskRevision

deliver :: Haitatsu ()
deliver = do
  AWS.initialStatus
  taskRev <- AWS.registerTask
  AWS.updateService taskRev
  AWS.healthCheck taskRev

register :: Haitatsu ()
register = void $ AWS.registerTask

create :: AWS.TaskRevision -> Haitatsu ()
create = void . AWS.createService

healthCheck :: AWS.TaskRevision -> Haitatsu ()
healthCheck = AWS.healthCheck

updateService :: AWS.TaskRevision -> Haitatsu ()
updateService = AWS.updateService

runCommand :: Command -> Haitatsu ()
runCommand Deliver = deliver
runCommand RegisterTask = register
runCommand (CreateService taskRev) = create taskRev
runCommand (HealthCheck taskRev) = healthCheck taskRev
runCommand (UpdateService taskRev) = updateService taskRev


