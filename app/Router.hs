module Router
  ( Routes
  , router
  , get
  , post
  , routesWriter
  ) where

import           Controller                    (Action)
import           Controller.NotFoundController (notFound)
import           Network.Wai                   (Request, Response, pathInfo,
                                                requestMethod)
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           RIO.Writer                    (Writer, execWriter, tell)

routesWriter = execWriter

router :: Request -> Routes -> IO Response
router req routes = do
  let path = pathInfo req
  let method = requestMethod req
  let maybeAction =
        L.foldl'
          (\maybeAction_ (routeMethod, routePath, action) ->
             case maybeAction_ of
               Just action -> Just action
               Nothing ->
                 if (routeMethod == method) && (routePath == path)
                   then Just action
                   else Nothing)
          Nothing
          (execWriter routes)
  case maybeAction of
    Just action -> runRIO req action
    Nothing     -> runRIO req notFound

type RawPath = Text

type Path = [Text]

type HTTPMethod = ByteString

type Route = (HTTPMethod, Path, Action)

type Routes = Writer [Route] ()

rawToPath :: RawPath -> Path
rawToPath path = filter (/= "") $ T.split (== '/') path

get :: RawPath -> Action -> Routes
get path action = tell [("GET", rawToPath path, action)]

post :: RawPath -> Action -> Routes
post path action = tell [("POST", rawToPath path, action)]

delete :: RawPath -> Action -> Routes
delete path action = tell [("DELETE", rawToPath path, action)]

put :: RawPath -> Action -> Routes
put path action = tell [("PUT", rawToPath path, action)]
