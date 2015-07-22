module Haitatsu.DryRun where

import qualified  Data.DList as D
import qualified  Data.Text.IO as T
import            System.IO

import            Haitatsu.Types

dryRun :: RunData -> Haitatsu () -> IO ()
dryRun runData haitatsu = do
  let (result, log) = runDryRun (haitatsuDryRun haitatsu) runData

  forM_ log T.putStrLn

  case result of
    Right () -> pure ()
    Left except -> throwM except

