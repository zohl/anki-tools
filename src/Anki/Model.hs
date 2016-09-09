{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Anki.Model (
    ModelId(..)
  , Model(..)
  , ModelField(..)
  , ModelTemplate(..)
  ) where

import Anki.Common
import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Aeson (Value(..), encode, decode, FromJSON(..), genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions, (.:), withObject)
import Data.Char (toLower, isUpper, chr)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.HashMap.Strict (toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (FromRow(..), SQLData(..), field)
import Database.SQLite.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Text as T
import Debug.Trace


type ModelId = WeaklyTypedInt

-- | Model from col.models
data Model = Model {
    modelId        :: ModelId
  , modelCss       :: Value -- String?
  , modelDid       :: Value -- DeckId?
  , modelFlds      :: [ModelField]
  , modelLatexPre  :: Value -- TODO String?
  , modelLatexPost :: Value -- TODO String?
  , modelMod       :: ModificationTime
  , modelName      :: Value -- TODO String?
  , modelSortf     :: Value -- TODO Int?
  , modelTags      :: Value -- TODO: [wtf]?
  , modelTmpls     :: [ModelTemplate]
  , modelType      :: Value -- TODO Int?
  , modelUsn       :: Value -- TODO Int?
  , modelVers      :: Value -- TODO [wtf]?
  } deriving (Show, Eq, Generic)

instance FromJSON Model where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField [Model] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry modelId ModelIdInconsistent) f where


-- | Field from col.models.flds
data ModelField = ModelField {
    mfName   :: String
  , mfMedia  :: Value -- TODO [wtf]?
  , mfSticky :: Value -- TODO Bool?
  , mfRtl    :: Value -- TODO Bool?
  , mfOrd    :: Value -- TODO Int?
  , mfFont   :: Value -- TODO String?
  , mfSize   :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON ModelField where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Template from col.models.tmpls
data ModelTemplate = ModelTemplate {
    mtName  :: String
  , mtQfmt  :: Value -- TODO String?
  , mtDid   :: Value -- TODO DeckId?
  , mtBafmt :: Value -- TODO String?
  , mtAfmt  :: Value -- TODO String?
  , mtBqfmt :: Value -- TODO String?
  , mtOrd   :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON ModelTemplate where
  parseJSON = genericParseJSON dropPrefixOptions
