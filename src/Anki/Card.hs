{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Anki.Card (
    CardId
  , Card(..)
  ) where

import Anki.Common
import Anki.Deck
import Anki.Note
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


type CardId = Int

-- | Cards from cards table.
data Card = Card {
    cardId     :: CardId
  , cardNid    :: NoteId
  , cardDid    :: DeckId
  , cardOrd    :: Int   -- TODO check type
  , cardMod    :: ModificationTime
  , cardUsn    :: Int   -- TODO check type
  , cardType   :: Int   -- TODO check type
  , cardQueue  :: Int   -- TODO check type
  , cardDue    :: Int   -- TODO check type
  , cardIvl    :: Int   -- TODO check type
  , cardFactor :: Int   -- TODO check type
  , cardReps   :: Int   -- TODO check type
  , cardLapses :: Int   -- TODO check type
  , cardLeft   :: Int   -- TODO check type
  , cardOdue   :: Int   -- TODO check type
  , cardOdid   :: Int   -- TODO check type
  , cardFlags  :: Int   -- TODO check type
  , cardData   :: Text  -- TODO check type
  } deriving (Show, Eq, Generic)

instance FromRow Card where
  fromRow = Card
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
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
