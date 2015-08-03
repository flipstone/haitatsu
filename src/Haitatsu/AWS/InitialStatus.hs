module Haitatsu.AWS.InitialStatus
  ( initialStatus
  , InitialCheckFailure
  ) where

import            Data.Monoid
import qualified  Data.Text as T
import            Data.Typeable

import            Haitatsu.AWS.CurrentDeployment
import            Haitatsu.Types


initialStatus :: Haitatsu CurrentDeployment
initialStatus = do
  result <- getCurrentDeployment

  echo Normal "Checking that current deployment is already healthy"

  case result of
    Left err -> do
     echo Normal $ "  * Refusing to deploy because of an err while looking up current deployment."
     echo Normal $ "  * " <> err

     throwM $ InitialCheckFailure err

    Right deployment -> do
      if currentDesiredCount deployment == currentRunningCount deployment
        then do echo Normal "  = Current deployment is healthy"
                echo Normal ""
        else do let err =    "Only " <> showT (currentRunningCount deployment)
                          <> " running, but " <> showT (currentDesiredCount deployment)
                          <> " desired."

                echo Normal  "  * Refusing to deploy because the current deployment looks unhealthy"
                echo Normal ("  * " <> err)
                throwM $ InitialCheckFailure err

      pure deployment

newtype InitialCheckFailure = InitialCheckFailure T.Text
  deriving (Show, Typeable)

instance Exception InitialCheckFailure

showT = T.pack . show

