module Router2
  ( get
  , post
  , int
  , match
  , text
  , router
  , Routes
  , (</>)
  , (</)
  , (/>)
  ) where

import           Controller                    (Action)
import           Controller.NotFoundController (notFound)
import           Data.Attoparsec.Text          (IResult (..), Parser, char,
                                                digit, feed, many1, parse,
                                                string, takeWhile1)
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
get = methodPathParser "GET"

post :: Parser a -> (a -> Action) -> Routes
post = methodPathParser "POST"

methodPathParser :: Text -> Parser a -> (a -> Action) -> Routes
methodPathParser method pathParser action = do
  let parser = match method >> action <$> pathParser
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

(/>) :: Parser a -> Parser b -> Parser b
(/>) pa pb = do
  pa
  char '/'
  pb

(</) :: Parser a -> Parser b -> Parser a
(</) pa pb = do
  a <- pa
  char '/'
  pb
  return a

(</>) :: Parser a -> Parser b -> Parser (a, b)
(</>) pa pb = do
  a <- pa
  char '/'
  b <- pb
  return (a, b)
