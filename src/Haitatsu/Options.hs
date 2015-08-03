module Haitatsu.Options
  ( Options(..)
  , optRelativePath
  , runOptions
  ) where

import qualified  Data.Text as T
import            Options.Applicative
import            System.FilePath

import            Data.Aeson.Substitution
import            Haitatsu.Command
import            Haitatsu.Types

data Options = Options {
    optIsDryRun :: Bool
  , optIsDryRollback :: Bool
  , optConfigFile :: FilePath
  , optEnvironment :: Environment
  , optVerbosity :: Verbosity
  , optContext :: Context
  , optCommand :: Command
  }

optRelativePath :: Options -> FilePath -> FilePath
optRelativePath options path =
  if isAbsolute path
  then path
  else takeDirectory (optConfigFile options) </> path

options :: Parser Options
options = Options
  <$> switch
      ( long "dry-run"
     <> help "Don't actually do anything, but print what would be done"
      )

  <*> switch
      ( long "dry-rollback"
     <> help "Simulate a rollback during a dryrun"
      )

  <*> strOption
      ( long "file"
     <> short 'f'
     <> metavar "HAITATSU_FILE"
     <> value "haitatsu.yml"
     <> help "Specify the path to the haitatsu file to be run"
      )

  <*> (envString <$> strOption
      ( long "environment"
     <> short 'e'
     <> metavar "ENVIRONMENT"
     <> help "Which environment to delivery"
      ))

  <*> flag Normal Verbose
      ( long "verbose"
     <> short 'v'
     <> help "Be extra chatty about what's going on"
      )

  <*> option optParseContext
      ( long "context"
     <> short 'c'
     <> metavar "key1=value1,key2=value2,..."
     <> value (contextList [])
     <> help "Set context variables"
      )

  <*> subparser (
        command "deliver" (info (pure Deliver)
                                (progDesc "Deliver the app to ECS"))

     <> command "register" (info (pure RegisterTask)
                                 (progDesc "Register a the task with ECS"))

     <> command "create" (info createServiceOptions
                               (progDesc "Create a new ecs service"))

     <> command "update" (info updateServiceOptions
                               (progDesc "Update a the ecs service to run the given task revision"))

     <> command "check" (info healthCheckOptions
                              (progDesc "Check the deployment status of a task revision"))
     )

healthCheckOptions :: Parser Command
healthCheckOptions =
  HealthCheck <$> argument taskRevReader (metavar "family:revision")

createServiceOptions :: Parser Command
createServiceOptions =
  CreateService <$> argument taskRevReader (metavar "family:revision")

updateServiceOptions :: Parser Command
updateServiceOptions =
  UpdateService <$> argument taskRevReader (metavar "family:revision")

taskRevReader :: ReadM TaskRevision
taskRevReader = do
  s <- str
  case T.splitOn ":" (T.pack s) of
    [family, revision] ->
      pure $ TaskRevision {
          taskFamily = family
        , taskRevision = revision
        }

    _ -> readerError "Invalid task revision. Must be in the format family:revision"

runOptions :: (Options -> IO a) -> IO a
runOptions main = execParser opts >>= main
  where
    opts = info (helper <*> options)
        ( fullDesc
       <> header "haitatsu - a simple ECS delivery tool"
        )

optParseContext :: ReadM Context
optParseContext = do
  s <- str
  case parseContext $ T.pack s of
    Right context -> pure context
    Left err -> readerError err


