module Haitatsu.WetRun where

import            Network.AWS
import qualified  System.Environment as Env
import            System.FilePath

import            Haitatsu.AWS
import            Haitatsu.Types

wetRun :: RunData -> Haitatsu () -> IO ()
wetRun runData haitatsu = do
  let region = configRegion $ environmentConfig runData
  env <- newEnv region Discover
  runWetRun (haitatsuWetRun haitatsu) runData env

