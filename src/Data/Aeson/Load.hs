module Data.Aeson.Load
  ( loadAeson
  , UnrecognizedExtensionError
  , ParseError
  ) where

import            Control.Monad.Catch
import            Control.Monad.IO.Class
import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Lazy as LBS
import            Data.Typeable
import qualified  Data.Yaml as Yaml
import            System.FilePath

aesonEitherStrict :: Aeson.FromJSON a => BS.ByteString -> Either String a
aesonEitherStrict s = Aeson.eitherDecode $ LBS.fromChunks [s]

loadAeson :: (Aeson.FromJSON a, MonadThrow m, MonadIO m)
          => FilePath -> m a
loadAeson path = do
  parser <- case takeExtension path of
            ".json" -> pure aesonEitherStrict
            ".yml" -> pure Yaml.decodeEither
            ".yaml" -> pure Yaml.decodeEither
            ext -> throwM (UnrecognizedExtensionError ext)

  bytes <- liftIO $ BS.readFile path

  case parser bytes of
    Left err -> throwM (ParseError path err)
    Right a -> pure a


newtype UnrecognizedExtensionError = UnrecognizedExtensionError String
  deriving (Show, Typeable)

instance Exception UnrecognizedExtensionError

data ParseError = ParseError FilePath String
  deriving (Show, Typeable)

instance Exception ParseError

