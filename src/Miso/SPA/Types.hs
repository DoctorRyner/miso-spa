{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Miso.SPA.Types where

import           Data.Aeson
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text           as T
import           GHC.Generics
import           Miso.String

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

