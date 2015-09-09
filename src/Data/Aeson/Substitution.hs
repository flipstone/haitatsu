module Data.Aeson.Substitution
  ( Template
  , Context
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
import            Data.Attoparsec.Text
import qualified  Data.Attoparsec.Text as Atto
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.DList as D
import            Data.Hashable
import qualified  Data.HashMap.Strict as M
import            Data.Maybe
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Typeable
import            Data.Vector

type Variable = T.Text

newtype Context = Context (M.HashMap Variable T.Text)
  deriving (Show, Monoid)

contextMap :: M.HashMap Variable T.Text -> Context
contextMap = Context

contextList :: [(Variable, T.Text)] -> Context
contextList = contextMap . M.fromList

data StringTemplate =
    StringLit T.Text StringTemplate
  | StringVar Variable StringTemplate
  | StringEnd
  deriving (Show, Eq)

runTemplate :: Monad m
            => (Variable -> m T.Text) -> StringTemplate -> m T.Text
runTemplate lookup template =
    T.concat <$> D.toList <$> go template D.empty
  where
    go (StringLit s next) list = go next (D.snoc list s)
    go (StringVar v next) list = go next =<< (D.snoc list <$> lookup v)
    go StringEnd list = pure list

data Template =
    Literal Value
  | Substitution StringTemplate
  | TArray (Vector Template)
  | TObject (M.HashMap T.Text Template)
  deriving (Show, Eq)

substituteWith :: MonadThrow m
               => (Variable -> m T.Text)
               -> Template
               -> m Value
substituteWith _ (Literal val) =
  pure val

substituteWith lookup (Substitution template) =
  String <$> runTemplate lookup template

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
context =
   contextList <$> catMaybes <$> (entry `sepBy` separator)
 where
   separator = void (char ',') <|> endOfLine

entry :: Parser (Maybe (Variable, T.Text))
entry = (Just <$> keyValue)
    <|> (const Nothing <$> blank)

blank :: Parser ()
blank = skipWhile isHorizontalSpace

keyValue :: Parser (Variable, T.Text)
keyValue = do
  key <- contextString
  "="
  value <- contextString
  pure (key, value)


contextString :: Parser T.Text
contextString = takeWhile1 isValid
  where isValid ',' = False
        isValid '=' = False
        isValid '\n' = False
        isValid _ = True

