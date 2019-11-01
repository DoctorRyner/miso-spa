{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Miso.SPA.Types where

import           Data.Aeson
import qualified Data.HashMap.Strict          as HMap
import           Data.Proxy
import qualified Data.Text                    as T
import           GHC.Generics
import           Miso.String

#ifdef ghcjs_HOST_OS
#else
import           Data.Swagger                 hiding (Response)
import           Data.Swagger.Internal.Schema

instance ToParamSchema MisoString where
    toParamSchema _ = toParamSchema (Proxy :: Proxy String)

instance ToSchema MisoString where declareNamedSchema = plain . paramSchemaToSchema
#endif


data Response ok
    = Ok ok
    | HttpError MisoString Int
    deriving (Generic, Show)

instance ToJSON (Response Value)
instance FromJSON (Response Value)

type Locale = HMap.HashMap T.Text T.Text

data Route
    = Root
    | Route String
    deriving (Show, Eq)
