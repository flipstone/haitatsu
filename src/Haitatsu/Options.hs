module Haitatsu.Options
  ( Options(..)
  , optRelativePath
  , runOptions
  ) where

import qualified  Data.Text as T
import            Options.Applicative
import            System.FilePath

import            Data.Aeson.Substitution
import            Haitatsu.Types

data Options = Options {
    optIsDryRun :: Bool
  , optConfigFile :: FilePath
  , optEnvironment :: Environment
  , optVerbosity :: Verbosity
  , optContext :: Context
  }

optRelativePath :: Options -> FilePath -> FilePath
optRelativePath options path =
  if isAbsolute path
  then path
  else takeDirectory (optConfigFile options) </> path

options :: Parser Options
options = Options
  <$> switch
      ( long "dryrun"
     <> help "Don't actually do anything, but print what would be done"
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


