module Haitatsu.Types
  ( Haitatsu(..)
  , DryRun
  , DryRunLog
  , runDryRun

  , WetRun
  , runWetRun

  , Environment(..)
  , envString
  , stringEnv

  , EnvironmentConfig(..)
  , RunData(..)
  , echo
  , sendAWS

  , Verbosity(..)

  , module Control.Monad.Catch
  , module Control.Monad.Reader

  , module Data.Aeson.Substitution
  ) where

import qualified  Data.DList as D
import            Control.Monad.Catch
import            Control.Monad.Catch.Pure
import            Control.Monad.Identity
import            Control.Monad.Reader
import            Control.Monad.Trans.AWS
import            Control.Monad.Writer
import qualified  Data.Text as T

import            Data.Aeson.Substitution

newtype Environment = Environment T.Text
  deriving (Eq, Show, Ord)

envString :: String -> Environment
envString = Environment . T.pack

stringEnv :: Environment -> String
stringEnv (Environment t) = T.unpack t

data EnvironmentConfig = EnvironmentConfig {
    taskDefinitionTemplateFile :: FilePath
  , clusterName :: T.Text
  , serviceName :: T.Text
  , configRegion :: Region
  , desiredCount :: Int
  , healthyWaitTimeSeconds :: Int
  , configContext :: Context
  } deriving Show

data Verbosity = Normal | Verbose
  deriving (Eq, Show, Ord)

data RunData = RunData {
    environmentConfig :: EnvironmentConfig
  , taskDefinitionTemplate :: Template
  , verbosity :: Verbosity
  } deriving Show

type DryRunLog = D.DList T.Text

newtype DryRun a = DryRun (ReaderT RunData
                          (CatchT
                          (Writer DryRunLog))
                          a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader RunData
           , MonadWriter DryRunLog
           , MonadThrow
           )

runDryRun :: DryRun a -> RunData -> (Either SomeException a, DryRunLog)
runDryRun (DryRun r) runData = runWriter (runCatchT (runReaderT r runData))

newtype WetRun a = WetRun (ReaderT RunData AWS a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader RunData
           , MonadIO
           , MonadThrow
           )

runWetRun :: WetRun a -> RunData -> Env -> IO a
runWetRun (WetRun r) runData env =
  either throwM pure =<<
  runAWST env (runReaderT r runData)

data Haitatsu a = Haitatsu {
    haitatsuDryRun :: DryRun a
  , haitatsuWetRun :: WetRun a
  }

instance Functor Haitatsu where
  fmap f h = Haitatsu (fmap f $ haitatsuDryRun h)
                      (fmap f $ haitatsuWetRun h)

instance Applicative Haitatsu where
  pure a = Haitatsu (pure a) (pure a)
  hF <*> hA = Haitatsu (haitatsuDryRun hF <*> haitatsuDryRun hA)
                       (haitatsuWetRun hF <*> haitatsuWetRun hA)

instance Monad Haitatsu where
  hA >>= f = Haitatsu (haitatsuDryRun hA >>= haitatsuDryRun . f)
                      (haitatsuWetRun hA >>= haitatsuWetRun . f)

instance MonadReader RunData Haitatsu where
  ask = Haitatsu ask ask
  local f hA = Haitatsu (local f $ haitatsuDryRun hA)
                        (local f $ haitatsuWetRun hA)

instance MonadThrow Haitatsu where
  throwM e = Haitatsu (throwM e) (throwM e)

echo :: Verbosity -> T.Text -> Haitatsu ()
echo v s = do
  currentVerbosity <- asks verbosity

  when (currentVerbosity >= v) $
    Haitatsu (tell $ D.singleton s)
             (liftIO $ putStrLn (T.unpack s))

sendAWS :: AWSRequest a => a -> WetRun (Rs a)
sendAWS = WetRun . lift . send

