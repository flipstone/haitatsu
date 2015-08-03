module Haitatsu.Run where

import            Data.Aeson.Load
import            Data.Monoid
import            System.Exit

import            Haitatsu.AWS
import            Haitatsu.Command
import            Haitatsu.Config
import            Haitatsu.DryRun
import            Haitatsu.Options
import            Haitatsu.Types
import            Haitatsu.WetRun

runHaitatsu :: Options -> IO ()
runHaitatsu options = do
  config <- loadConfig (optConfigFile options)
  runData <- loadRunData options config

  let runner = if optIsDryRun options
               then dryRun
               else wetRun

  runner runData $ runCommand (optCommand options)

runMain :: IO ()
runMain =
  runOptions runHaitatsu `catches` [
      Handler handleInitialCheckFailure
    ]

statusInitialCheckFailure :: Int
statusInitialCheckFailure = 4

statusNoSuchEnv :: Int
statusNoSuchEnv = 3

handleInitialCheckFailure :: InitialCheckFailure -> IO ()
handleInitialCheckFailure _ = exitWith (ExitFailure statusInitialCheckFailure)

loadRunData :: Options -> HaitatsuConfig -> IO RunData
loadRunData options config = do
  let env = optEnvironment options

  envConfig <- maybe (missingEnv env) pure (getEnvConfig env config)
  template <- loadAeson (optRelativePath options $
                         taskDefinitionTemplateFile envConfig)

  pure $ RunData (augmentContext options envConfig)
                 template
                 (optVerbosity options)
                 (optIsDryRollback options)

augmentContext :: Options -> EnvironmentConfig -> EnvironmentConfig
augmentContext options config =
    config { configContext = context }
  where
    context = optContext options <> configContext config

missingEnv :: Environment -> IO a
missingEnv env = do
  putStr "No such environment: "
  putStrLn (stringEnv env)
  exitWith (ExitFailure statusNoSuchEnv)

