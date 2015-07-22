module Haitatsu.AWS.Credentials
  ( loadCredsFile
  ) where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.Trans.AWS
import            Data.Attoparsec.ByteString
import qualified  Data.Attoparsec.ByteString.Char8 as AtChar
import qualified  Data.ByteString as BS

credLineEnd :: Parser ()
credLineEnd = do
  void (AtChar.endOfLine <|> AtChar.endOfInput)

parseCredLine :: BS.ByteString -> Parser BS.ByteString
parseCredLine key = do
  void $ string key
  skipWhile AtChar.isHorizontalSpace
  void $ AtChar.char '='
  skipWhile AtChar.isHorizontalSpace
  value <- AtChar.takeWhile (not . AtChar.isSpace)
  skipWhile AtChar.isHorizontalSpace
  credLineEnd
  pure value

parseCreds :: Parser Credentials
parseCreds = do
  AtChar.skipWhile AtChar.isSpace
  string "[default]" >> AtChar.endOfLine
  keyId <- parseCredLine "aws_access_key_id"
  secret <- parseCredLine "aws_secret_access_key"
  return (FromKeys (AccessKey keyId) (SecretKey secret))

loadCredsFile :: FilePath -> IO Credentials
loadCredsFile filePath = do
  text <- BS.readFile filePath
  either (\msg -> error $ "Error parsing creds file: " ++ msg)
         return
         (parseOnly (parseCreds <* endOfInput) text)

