{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Anki.Deck (
    DeckOptions(..)
  , DeckOptionsId
  , DeckOptionsLapse(..)
  , DeckOptionsRev(..)
  , DeckOptionsNew(..)

  , Deck(..)
  , DeckId
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


type DeckOptionsId = WeaklyTypedInt


data DeckOptions = DeckOptions {
    doId       :: DeckOptionsId
  , doAutoplay :: Value -- TODO Bool?
  , doDyn      :: Value -- TODO Bool?
  , doLapse    :: DeckOptionsLapse  -- ^ TODO
  , doMaxTaken :: Value -- TODO Int?
  , doMod      :: ModificationTime
  , doName     :: Value -- TODO String?
  , doNew      :: DeckOptionsNew    -- ^ TODO
  , doReplayq  :: Value -- TODO Bool?
  , doRev      :: DeckOptionsRev    -- ^ TODO
  , doTimer    :: Value -- TODO Int/Bool?
  , doUsn      :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptions where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField [DeckOptions] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry doId DeckOptionsIdInconsistent) f


-- | Volatile fields of col.decks
data DeckExtension
  = NormalDeck {
      deckBrowserCollapsed  :: Value   -- TODO Bool?
    , deckConf              :: Value   -- TODO DeckOptionsId?
    , deckExtendNew         :: Value   -- TODO Int?
    , deckExtendRev         :: Value   -- TODO Int?
    }
  | DynamicDeck {
      deckDelays            :: Value   -- TODO Int?
    , deckResched           :: Value   -- TODO Bool?
    , deckReturn            :: Value   -- TODO Bool?
    , deckSeparate          :: Value   -- TODO Bool?
    , deckTerms             :: Value   -- TODO [[wtf]]?
    } deriving (Show, Eq, Generic)

instance FromJSON DeckExtension where
  parseJSON = withObject "DeckExtension" $ \o -> getBool <$> (o .: "dyn") >>= \case
      False -> do
        deckBrowserCollapsed <- o .: "browserCollapsed"
        deckConf             <- o .: "conf"
        deckExtendNew        <- o .: "extendNew"
        deckExtendRev        <- o .: "extendRev"
        return NormalDeck {..}

      True -> do
        deckDelays   <- o .: "delays"
        deckResched  <- o .: "resched"
        deckReturn   <- o .: "return"
        deckSeparate <- o .: "separate"
        deckTerms    <- o .: "terms"
        return DynamicDeck {..}


-- | Options from cols.deck.dconf.lapse.
data DeckOptionsLapse = DeckOptionsLapse {
    dolLeechFails  :: Value -- TODO Int?
  , dolMinInt      :: Value -- TODO Int?
  , dolDelays      :: Value -- TODO [Int]?
  , dolLeechAction :: Value -- TODO Int/Enum?
  , dolMult        :: Value -- TODO Double?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptionsLapse where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.new.
data DeckOptionsNew = DeckOptionsNew {
    donPerDay        :: Value -- TODO Int?
  , donDelays        :: Value -- TODO (Int, Int)?
  , donSeparate      :: Value -- TODO Bool?
  , donInts          :: Value -- TODO (Int, Int, Int)?
  , donInitialFactor :: Value -- TODO Double?
  , donBury          :: Value -- TODO Bool?
  , donOrder         :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptionsNew where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.rev
data DeckOptionsRev = DeckOptionsRev {
    dorPerDay   :: Value -- TODO Int?
  , dorFuzz     :: Value -- TODO Double?
  , dorIvlFct   :: Value -- TODO Double?
  , dorMaxIvl   :: Value -- TODO Double?
  , dorEase4    :: Value -- TODO Double?
  , dorBury     :: Value -- TODO Bool?
  , dorMinSpace :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptionsRev where
  parseJSON = genericParseJSON dropPrefixOptions



type DeckId = WeaklyTypedInt

data Deck = Deck {
    deckId                :: DeckId
  , deckName              :: Value         -- TODO String?
  , deckCollapsed         :: Value         -- TODO Bool?
  , deckDesc              :: Value         -- TODO String?
  , deckMod               :: ModificationTime
  , deckUsn               :: Value         -- TODO Int?
  , deckLrnToday          :: Value         -- TODO (Int, Int)?
  , deckNewToday          :: Value         -- TODO (Int, Int)?
  , deckRevToday          :: Value         -- TODO (Int, Int)?
  , deckTimeToday         :: Value         -- TODO (Int, Int)?
  , deckExtension         :: DeckExtension
  } deriving (Show, Eq, Generic)


instance FromJSON Deck where
  parseJSON = withObject "Deck" $ \o -> do
    deckName      <- o .: "name"
    deckCollapsed <- o .: "collapsed"
    deckDesc      <- o .: "desc"
    deckId        <- o .: "id"
    deckMod       <- o .: "mod"
    deckUsn       <- o .: "usn"
    deckLrnToday  <- o .: "lrnToday"
    deckNewToday  <- o .: "newToday"
    deckRevToday  <- o .: "revToday"
    deckTimeToday <- o .: "timeToday"
    deckExtension <- parseJSON $ Object o
    return Deck {..}

instance FromField [Deck] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry deckId DeckIdInconsistent) f
