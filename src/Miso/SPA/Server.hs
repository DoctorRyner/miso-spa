{-# LANGUAGE CPP #-}

module Miso.SPA.Server where

#ifndef ghcjs_HOST_OS
import           Data.ByteString.Lazy
import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Run.Files  (indexHtml)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Static
import           Network.WebSockets                     hiding (Response)

indexResponse :: Response
indexResponse = responseLBS status200 [("Content-Type", "text/html")] indexHtml

spaWithJS :: ByteString -> Application
spaWithJS js req sendResponse = case (requestMethod req, pathInfo req) of
    ("GET", [ "jsaddle.js" ]) -> sendResponse $ responseLBS status200 [("Content-Type", "application/javascript")] js
    ("GET", _)                -> sendResponse indexResponse
    _                         -> sendResponse $ responseLBS status403 [("Content-Type", "text/plain")] "Frontend doesn't support this"

spa :: Application
spa = spaWithJS $ jsaddleJs False

spaDebug :: Application
spaDebug = spaWithJS $ jsaddleJs True
#endif

#ifdef ghcjs_HOST_OS
run :: Int -> IO () -> IO ()
run _ = id
#else
run :: Int -> JSM () -> IO ()
run port f = runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
    jsaddleOr defaultConnectionOptions (f >> syncPoint) (staticPolicy (addBase "./static/") $ simpleCors spa)
#endif

#ifdef ghcjs_HOST_OS
debug :: Int -> IO () -> IO ()
debug = error "debug server is for GHC only"
#else
debug :: Int -> JSM () -> IO ()
debug port f =
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions
                (registerContext >> f >> syncPoint)
                (staticPolicy (addBase "./static/") $ simpleCors $ withRefresh spaDebug)
#endif
