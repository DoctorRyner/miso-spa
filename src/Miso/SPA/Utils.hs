module Miso.SPA.Utils
    ( module Miso.SPA.Utils
    , Route (..)
    ) where

import           Control.Exception           (Exception)
import           Control.Monad               (liftM)
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.HashMap.Lazy           as HMap
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle hiding (JSM, (!!), (<#))
import           Miso
import           Miso.SPA.Types
import           Miso.String                 (MisoString, ms, unpack)
import qualified Miso.String                 as MS
import           Network.URI                 as URI

mshow :: Show a => a -> MisoString
mshow = ms . show

(<>~) :: Show a => MisoString -> a -> MisoString
(<>~) str = (str <>) . mshow

logJS :: MisoString -> JSM ()
logJS str = consoleLog =<< val (unpack str)

logJS' :: Show a => a -> JSM ()
logJS' str = consoleLog =<< val (show str)

withJS :: model -> JSM action -> Effect action model
withJS = (<#)

withJS_ :: model -> JSM () -> Effect action model
withJS_ m a = effectSub m $ const $ a >> pure ()

withIO :: model -> IO action -> Effect action model
withIO model = (model <#) . liftIO

withIO_ :: model -> IO () -> Effect action model
withIO_ model action = withJS_ model $ liftIO action

maybeStyle :: Maybe MisoString -> View action
maybeStyle = \case
    Just cssText -> nodeHtml "style" [] [ text cssText ]
    Nothing      -> ""

try :: (MonadCatch m, Exception e) => m b -> m (Either e b)
try a = catch (Right `liftM` a) (return . Left)

fromJS :: MisoString -> JSM JSVal
fromJS = eval

fromJS' :: [MisoString] -> JSM JSVal
fromJS' = eval . MS.unlines

deleteFirst :: Eq t => t -> [t] -> [t]
deleteFirst _ [] = []
deleteFirst a (b:bc) | a == b    = bc
                     | otherwise = b : deleteFirst a bc

route :: (URI -> action) -> MisoString -> URI -> action
route action routeStr uri = action $ uri { URI.uriPath = unpack routeStr }

fromResp :: Response response -> model -> (response -> model) -> Effect action model
fromResp response model updator = case response of
    Ok resp         -> pure $ updator resp
    HttpError err _ -> model `withJS_` logJS err

fromRespDebug :: Response response -> model -> (response -> JSM model) -> Effect action model
fromRespDebug response model updator = case response of
    Ok resp         -> effectSub model $ const $ updator resp >> pure ()
    HttpError err _ -> model `withJS_` logJS err

uriToRouteString :: URI -> String
uriToRouteString = eraseSlashAtPathEdges . uriPath where
    eraseSlashAtPathEdges str = if str == "" || str == "/"
        then ""
        else tail $ (if last str == '/' then init else id) str

router :: URI -> Route
router uri = case uriToRouteString uri of
    ""    -> Root
    route -> Route route

-- Extract value from locale
fromLocale :: MisoString -> Locale -> MisoString
fromLocale key' locale = (\key -> ms $ HMap.lookupDefault "" key locale) . T.pack $ unpack key'

(<|) :: MisoString -> Locale -> MisoString
(<|) = fromLocale
