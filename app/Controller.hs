module Controller
  ( Action
  , HasRequest
  , responseLBS
  , lbs
  , maybeJson
  , text
  , responseBuilder
  , json
  , RequestBodyLength(..)
  , requestMethod
  , httpVersion
  , rawPathInfo
  , rawQueryString
  , requestHeaders
  , isSecure
  , remoteHost
  , pathInfo
  , queryString
  , requestBodyChunk
  , vault
  , requestBodyLength
  , requestHeaderHost
  , requestHeaderRange
  , requestHeaderReferer
  , requestHeaderUserAgent
  ) where

import           Data.Aeson          (FromJSON, ToJSON, encode)
import           Data.Maybe          (fromMaybe)
import           Data.Vault.Lazy     (Vault)
import           Network.HTTP.Types
import           Network.Socket      (SockAddr)
import           Network.Wai         (Application, Request,
                                      RequestBodyLength (..), Response,
                                      ResponseReceived, responseBuilder,
                                      responseLBS)
import qualified Network.Wai         as Wai
import           RIO
import           RIO.ByteString.Lazy (fromStrict)
import           RIO.Text            (Text, encodeUtf8)

type HasRequest a = RIO Request a

type Action = RIO Request Response

lbs :: LByteString -> Action
lbs bs = pure $ responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] bs

json :: (FromJSON a, ToJSON a) => a -> Action
json json = pure $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (encode json)

maybeJson :: (FromJSON a, ToJSON a) => Maybe a -> Action
maybeJson json =
  pure $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (maybe "null" encode json)

text :: Text -> Action
text text = pure $ responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] (textToLBS text)

textToLBS :: Text -> LByteString
textToLBS = fromStrict . encodeUtf8

requestMethod :: HasRequest Method
requestMethod = Wai.requestMethod <$> ask

httpVersion :: HasRequest HttpVersion
httpVersion = Wai.httpVersion <$> ask

rawPathInfo :: HasRequest ByteString
rawPathInfo = Wai.rawPathInfo <$> ask

rawQueryString :: HasRequest ByteString
rawQueryString = Wai.rawQueryString <$> ask

requestHeaders :: HasRequest RequestHeaders
requestHeaders = Wai.requestHeaders <$> ask

isSecure :: HasRequest Bool
isSecure = Wai.isSecure <$> ask

remoteHost :: HasRequest SockAddr
remoteHost = Wai.remoteHost <$> ask

pathInfo :: HasRequest [Text]
pathInfo = Wai.pathInfo <$> ask

queryString :: HasRequest Query
queryString = Wai.queryString <$> ask

requestBodyChunk :: HasRequest ByteString
requestBodyChunk = do
  req <- ask
  liftIO (Wai.getRequestBodyChunk req)

vault :: HasRequest Vault
vault = Wai.vault <$> ask

requestBodyLength :: HasRequest RequestBodyLength
requestBodyLength = Wai.requestBodyLength <$> ask

requestHeaderHost :: HasRequest (Maybe ByteString)
requestHeaderHost = Wai.requestHeaderHost <$> ask

requestHeaderRange :: HasRequest (Maybe ByteString)
requestHeaderRange = Wai.requestHeaderRange <$> ask

requestHeaderReferer :: HasRequest (Maybe ByteString)
requestHeaderReferer = Wai.requestHeaderReferer <$> ask

requestHeaderUserAgent :: HasRequest (Maybe ByteString)
requestHeaderUserAgent = Wai.requestHeaderUserAgent <$> ask
