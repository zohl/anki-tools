{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Anki.Collection (
    Collection(..)
  , GlobalOptions(..)
  , Tag(..)
  ) where

import Anki.Common
import Anki.Deck
import Anki.Model
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


-- | Collection as in col table
data Collection = Collection {
    collectionId            :: Int            -- ^ collection identifier (id)
  , collectionCrt           :: Int            -- TODO check type
  , collectionMod           :: ModificationTime
  , collectionScm           :: Int            -- TODO check type
  , collectionVer           :: Int            -- TODO check type
  , collectionDty           :: Int            -- TODO check type
  , collectionUsn           :: Int            -- TODO check type
  , collectionLs            :: Int            -- TODO check type
  , collectionGlobalOptions :: GlobalOptions  -- ^ global options (col.conf)
  , collectionModels        :: [Model]        -- ^ models in the collection(col.models)
  , collectionDecks         :: [Deck]         -- ^ decks in the collection (col.decks)
  , collectionDeckOptions   :: [DeckOptions]  -- ^ deck options (col.dconf)
  , collectionTags          :: [Tag]          -- ^ tags (col.tags)
  } deriving (Show, Eq, Generic)

instance FromRow Collection where
  fromRow = Collection
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field


-- | Global opitions
data GlobalOptions = GlobalOptions {
   goNextPos       :: Value     -- TODO Int?
 , goEstTimes      :: Value     -- TODO Bool?
 , goSortBackwards :: Value     -- TODO Bool?
 , goSortType      :: Value     -- TODO String?
 , goTimeLim       :: Value     -- TODO Int?
 , goActiveDecks   :: [DeckId]  -- ^ TODO
 , goAddToCur      :: Value     -- TODO Bool?
 , goCurDeck       :: Value     -- TODO DeckId?
 , goCurModel      :: ModelId   -- ^ TODO
 , goLastUnburied  :: Value     -- TODO Int?
 , goCollapseTime  :: Value     -- TODO Int?
 , goActiveCols    :: Value     -- TODO [String]?
 , goSavedFilters  :: Value     -- TODO [wtf]?
 , goDueCounts     :: Value     -- TODO Bool?
 , goNewBury       :: Value     -- TODO Bool?
 , goNewSpread     :: Value     -- TODO Int?
 } deriving (Show, Eq, Generic)

instance FromJSON GlobalOptions where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField GlobalOptions where
  fromField f = getTextValue f >>= maybe (throwErr f WrongJsonFormat) return . decode


-- | Tags from col.tags.
data Tag = Tag {
    tagName   :: String -- ^ tag name (key)
  , tagNumber :: Int    -- ^ TODO: wtf? (value)
  } deriving (Show, Eq)

instance FromField [Tag] where
  fromField f = getJsonValue f >>= fromDictionary mkTag f where
    mkTag :: Field -> (Text, Value) -> Ok Tag
    mkTag f' = \case
      (name, Number number) -> return $ Tag (T.unpack name) (round number)
      _                     -> throwErr f' WrongJsonFormat

