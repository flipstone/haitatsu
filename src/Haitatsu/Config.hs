module Haitatsu.Config where

import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Data.Aeson
import            Data.Aeson.Load
import qualified  Data.HashMap.Strict as M
import qualified  Data.Text as T
import            Network.AWS

import            Haitatsu.Types

newtype Environments = Environments (M.HashMap T.Text EnvironmentConfig)
  deriving Show

data HaitatsuConfig = HaitatsuConfig {
    configFile :: FilePath
  , configEnvironments :: Environments
  } deriving Show

getEnvConfig :: Environment -> HaitatsuConfig -> Maybe EnvironmentConfig
getEnvConfig (Environment env) config =
  let Environments map = configEnvironments config
  in M.lookup env map

-- Config Loading

loadConfig :: (MonadThrow m, MonadIO m)
           => FilePath -> m HaitatsuConfig
loadConfig path = HaitatsuConfig path <$> loadAeson path

-- JSON Instances

instance FromJSON Region where
  parseJSON (String t) =
    case reads (T.unpack t) of
    [(region,"")] -> pure region
    _ -> fail "Invalid Region."

  parseJSON _ = fail "Region must be a string"

instance FromJSON EnvironmentConfig where
  parseJSON (Object o) =
    EnvironmentConfig <$> (o .: "task_definition_template")
                      <*> (o .: "cluster_name")
                      <*> (o .: "service_name")
                      <*> (o .:? "service_role")
                      <*> (o .:? "load_balancers" .!= [])
                      <*> (o .: "region")
                      <*> (o .: "desired_count")
                      <*> (o .: "health_check_timeout_seconds")
                      <*> (o .:? "context" .!= contextList [])

  parseJSON _ = fail "Invalid environment config"

instance FromJSON LoadBalancerConfig where
  parseJSON (Object o) =
    LoadBalancerConfig <$> (o .: "load_balancer_name")
                       <*> (o .: "container_name")
                       <*> (o .: "container_port")

  parseJSON _ = fail "Loadbalancer must have attributes."

instance FromJSON Context where
  parseJSON (Object o) = contextMap <$> traverse parseJSON o
  parseJSON _ = fail "Environment context must have attributes."

instance FromJSON Environments where
  parseJSON (Object o) = Environments <$> traverse parseJSON o
  parseJSON _ = fail "Invalid Haitatsu file."

