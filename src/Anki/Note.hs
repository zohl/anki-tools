{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Anki.Note (
    NoteId
  , Note(..)
  , NoteField(..)
  ) where

import Anki.Common
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


type NoteId = Int

fieldSeparator :: Char
fieldSeparator = chr 0x1F

-- | Notes from notes table.
data Note = Note {
    noteId    :: NoteId
  , noteGuid  :: String
  , noteMid   :: ModelId
  , noteMod   :: ModificationTime
  , noteUsn   :: Int -- TODO check type
  , noteTags  :: String -- TODO ?
  , noteFlds  :: [NoteField]
  , noteSfld  :: NoteField
  , noteCsum  :: Int -- TODO check type
  , noteFlags :: Int -- TODO check type
  , noteData  :: String -- TODO check type
  } deriving (Show, Eq, Generic)

instance FromRow Note where
  fromRow = Note
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

newtype NoteField = NoteField { getNoteField :: Text } deriving (Show, Eq)

instance FromField NoteField where
  fromField f = NoteField <$> fromField f

instance FromField [NoteField] where
  fromField f = (fmap NoteField . T.split (== fieldSeparator)) <$> fromField f

