module Haitatsu.AWS.RegisterTask where

import            Control.Lens
import            Control.Monad.Trans.AWS
import            Data.Aeson
import qualified  Data.Aeson as Aeson
import            Data.Monoid
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as E
import            Data.Typeable
import qualified  Data.Yaml as Yaml
import            Network.AWS.ECS

import            Haitatsu.Types

data TaskRevision = TaskRevision {
    taskFamily :: T.Text
  , taskRevision :: T.Text
  }

formatRevision :: TaskRevision -> T.Text
formatRevision t = taskFamily t <> ":" <> taskRevision t

registerTask :: Haitatsu TaskRevision
registerTask =
    do echo Normal "Registering ECS Task Definition..."

       taskDef <- buildTaskDefinition

       echo Verbose ""
       echo Verbose "-- Task Definition --"
       echo Verbose $ E.decodeUtf8 $ Yaml.encode taskDef

       rev <- register taskDef

       echo Normal ("  = registered task revision " <> formatRevision rev)
       echo Normal ""
       pure rev
  where
    register req = Haitatsu (dry req) (wet req)

    dry :: RegisterTaskDefinition -> DryRun TaskRevision
    dry req = do let fam = req ^. rtdFamily
                 pure (TaskRevision fam "dryrun")

    wet :: RegisterTaskDefinition -> WetRun TaskRevision
    wet req = do rs <- sendAWS req
                 taskDef <- require "taskDefinition" (rs ^. rtdrsTaskDefinition)
                 fam <- require "family" (taskDef ^. tdFamily)
                 rev <- require "revision" (taskDef ^. tdRevision)

                 pure (TaskRevision fam (T.pack (show rev)))

    require msg value = maybe (throwM $ ResponseValueMissingError msg)
                              pure
                              value

buildTaskDefinition :: (MonadThrow m, MonadReader RunData m)
                    => m RegisterTaskDefinition
buildTaskDefinition = do
  template <- asks taskDefinitionTemplate
  config <- asks environmentConfig
  json <- substitute (configContext config) template

  case fromJSON json of
    Aeson.Error err -> throwM (IncompleteTaskDefinitionError err)
    Aeson.Success value -> pure value


instance FromJSON RegisterTaskDefinition where
  parseJSON (Object o) =
    ((rtdContainerDefinitions .~) <$> (o .: "containerDefinitions"))
      <*>
    (((rtdVolumes .~) <$> (o .:? "volumes" .!= []))
      <*>
     (registerTaskDefinition <$> (o .: "family")))

  parseJSON _ = fail "Task Definition must be an object"

newtype IncompleteTaskDefinitionError = IncompleteTaskDefinitionError String
  deriving (Show, Typeable)

instance Exception IncompleteTaskDefinitionError

newtype ResponseValueMissingError = ResponseValueMissingError String
  deriving (Show, Typeable)

instance Exception ResponseValueMissingError

