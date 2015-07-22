module Haitatsu.AWS.InitialStatus
  ( initialStatus
  , InitialCheckFailure
  ) where

import            Data.Monoid
import qualified  Data.Text as T
import            Data.Typeable

import            Haitatsu.AWS.RegisterTask
import            Haitatsu.AWS.TaskStatus
import            Haitatsu.Types


initialStatus :: Haitatsu ()
initialStatus = do
  echo Normal ("Checking that existing deployment is already healthy")
  status <- getTaskStatus primary

  case status of
    Right () -> do
      echo Normal "  = Current deployment is healthy"
      echo Normal ""

    Left err -> do
      echo Normal  "  * Refusing to deploy because the current deployment looks unhealthy"
      echo Normal ("  * " <> T.pack err)
      throwM $ InitialCheckFailure err

newtype InitialCheckFailure = InitialCheckFailure String
  deriving (Show, Typeable)

instance Exception InitialCheckFailure

