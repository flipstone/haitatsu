module Haitatsu.WetRun where

import            Network.AWS
import qualified  System.Environment as Env
import            System.FilePath

import            Haitatsu.AWS
import            Haitatsu.Types

wetRun :: RunData -> Haitatsu () -> IO ()
wetRun runData haitatsu = do
  let region = configRegion $ environmentConfig runData
  env <- loadAWSEnv region
  runWetRun (haitatsuWetRun haitatsu) runData env

loadAWSEnv :: Region -> IO Env
loadAWSEnv region = do
  home <- Env.getEnv "HOME"
  creds <- loadCredsFile (home </> ".aws" </> "credentials")
  getEnv region creds

