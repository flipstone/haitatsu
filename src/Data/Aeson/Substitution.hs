module Data.Aeson.Substitution
  ( Template
  , Context
  , ContextValue (..)
  , contextList
  , contextMap
  , parseContext
  , substitute
  , substituteWith
  ) where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Data.Aeson
import            Data.Aeson.Types (typeMismatch)
import            Data.Attoparsec.Combinator
import            Data.Attoparsec.Text
import qualified  Data.Attoparsec.Text as Atto
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.DList as D
import            Data.Hashable
import qualified  Data.HashMap.Strict as M
import            Data.Maybe
import            Data.Monoid
import            Data.Scientific
import qualified  Data.Text as T
import            Data.Typeable
import            Data.Vector

type Variable = T.Text

data ContextValue =
    ContextInt Integer
  | ContextString T.Text
  deriving Show

forceTextValue :: ContextValue -> T.Text
forceTextValue (ContextString t) = t
forceTextValue (ContextInt i) = T.pack $ show i

instance ToJSON ContextValue where
  toJSON (ContextInt i) = toJSON i
  toJSON (ContextString t) = toJSON t

instance FromJSON ContextValue where
  parseJSON (String t) = pure $ ContextString t
  parseJSON (Number s) =
    case floatingOrInteger s of
      Right i -> pure $ ContextInt i
      Left _ -> fail $ "Only strings and integers may be used as context values, not " <> show s

  parseJSON j = typeMismatch "Only strings and integers may be used as context values" j


newtype Context = Context (M.HashMap Variable ContextValue)
  deriving (Show, Monoid)

contextMap :: M.HashMap Variable ContextValue -> Context
contextMap = Context

contextList :: [(Variable, ContextValue)] -> Context
contextList = contextMap . M.fromList

data StringTemplate =
    StringLit T.Text StringTemplate
  | StringVar Variable StringTemplate
  | StringEnd
  deriving (Show, Eq)

runTemplate :: Monad m
            => (Variable -> m ContextValue) -> StringTemplate -> m ContextValue
runTemplate lookup template = do
    parts <- D.toList <$> go template D.empty

    pure $ case parts of
           [justOne] -> justOne
           _ -> ContextString $ T.concat $ fmap forceTextValue parts
  where
    go (StringLit s next) list = go next (D.snoc list (ContextString s))
    go (StringVar v next) list = go next =<< (D.snoc list <$> lookup v)
    go StringEnd list = pure list

data Template =
    Literal Value
  | Substitution StringTemplate
  | TArray (Vector Template)
  | TObject (M.HashMap T.Text Template)
  deriving (Show, Eq)

substituteWith :: MonadThrow m
               => (Variable -> m ContextValue)
               -> Template
               -> m Value
substituteWith _ (Literal val) =
  pure val

substituteWith lookup (Substitution template) =
  toJSON <$> runTemplate lookup template

substituteWith lookup (TArray vec) =
  Array <$> traverse (substituteWith lookup) vec

substituteWith lookup (TObject hm) =
  Object <$> traverse (substituteWith lookup) hm

newtype MissingVariableError = MissingVariableError Variable
  deriving (Show, Typeable)

instance Exception MissingVariableError

substitute :: MonadThrow m => Context -> Template -> m Value
substitute (Context varMap) = substituteWith lookupVar
  where lookupVar v =
          case M.lookup v varMap of
          Just val -> pure val
          Nothing -> throwM (MissingVariableError v)

-- Parse a Template from JSON

endOfTemplate :: Parser StringTemplate
endOfTemplate =
  endOfInput *> pure StringEnd

literal :: Parser StringTemplate
literal =
  StringLit <$> takeWhile1 (not . (== '$'))
            <*> stringTemplate

escapedLiteral :: Parser StringTemplate
escapedLiteral =
  StringLit "$" <$> ("$" *> literal)

dollarMessage :: String
dollarMessage =
  "Invalid '$' in template value. \
  \Use ${varname} if you intended a variable substitution. \
  \If you intended to have a string value beginning with '$' \
  \rather than a template variable, use '$$' to indicate this."

variable :: Parser StringTemplate
variable =
  StringVar <$> ("{" *> Atto.takeWhile1 (not . (== '}')) <* "}")
            <*> stringTemplate

dollarExpression :: Parser StringTemplate
dollarExpression =
  "$" *> (variable <|> escapedLiteral <|> fail dollarMessage)

stringTemplate :: Parser StringTemplate
stringTemplate = literal <|> dollarExpression <|> endOfTemplate

instance FromJSON Template where
  parseJSON (String s) = either fail (pure . Substitution)
                       $ parseOnly stringTemplate s

  parseJSON (Array vec) = TArray <$> traverse parseJSON vec
  parseJSON (Object hm) = TObject <$> traverse parseJSON hm
  parseJSON val = pure $ Literal val

parseContext :: T.Text -> Either String Context
parseContext = parseOnly (context <* endOfInput)

context :: Parser Context
context = contextList <$> catMaybes <$> (entry `sepBy` itemSeparator)

itemSeparator :: Parser ()
itemSeparator = void (char ',') <|> endOfLine

entry :: Parser (Maybe (Variable, ContextValue))
entry = (Just <$> keyValue)
    <|> (const Nothing <$> blank)

blank :: Parser ()
blank = skipWhile isHorizontalSpace

keyValue :: Parser (Variable, ContextValue)
keyValue = do
  key <- contextString
  "="
  value <- contextValue
  pure (key, value)


contextString :: Parser T.Text
contextString = takeWhile1 isValid
  where isValid ',' = False
        isValid '\n' = False
        isValid '=' = False
        isValid '"' = False
        isValid _ = True

contextInt :: Parser Integer
contextInt = decimal <* lookAhead (itemSeparator <|> endOfInput)

contextValue :: Parser ContextValue
contextValue =
     (ContextString <$> quotedString)
 <|> (ContextInt <$> contextInt)
 <|> (ContextString <$> contextString)

quotedString :: Parser T.Text
quotedString = char '"' *> contextString <* char '"'

