module Router2 where

import           Controller                    (Action)
import           Controller.NotFoundController (notFound)
import           Data.Attoparsec.Text          hiding (match)
import           Data.Typeable                 (TypeRep)
import           Network.HTTP.Types
import           Network.Wai                   (Request, Response, rawPathInfo,
                                                requestMethod)
import           Prelude                       (read)
import           RIO
import           RIO.Text                      (Text, pack)
import qualified RIO.Text                      as T
import           RIO.Writer                    (Writer, execWriter, tell)

type Routes = ReaderT Text (Either Action) ()

router :: Request -> Routes -> Action
router req routes = do
  let route = decodeUtf8With lenientDecode $ requestMethod req <> rawPathInfo req
  case runReaderT routes route of
    Right _     -> notFound
    Left action -> action

get :: Parser a -> (a -> Action) -> Routes
get pathParser action = do
  let parser = match "GET" >> action <$> pathParser
  route <- ask
  lift $
    case parse parser route `feed` "" of
      Done "" action -> Left action
      _              -> Right ()

int :: Parser Int
int = read <$> many1 digit

text :: Parser Text
text = takeWhile1 (/= '/')

match :: Text -> Parser ()
match txt = string txt *> return ()