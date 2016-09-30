{-|
  Module:      Anki.Deck
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Representation of a deck and related definitions.
-}

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
  , DeckExtension(..)
  , DeckId
  ) where


import Anki.Common (WeaklyTypedInt, WeaklyTypedBool(..), ModificationTime, AnkiException(..))
import Anki.Common (dropPrefixOptions, getJsonValue, fromDictionary, mkEntry)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON)
import Data.Aeson.Types ((.:), withObject)
import Database.SQLite.Simple.FromField (FromField(..))
import GHC.Generics (Generic)

-- | Type for deck options ids.
type DeckOptionsId = WeaklyTypedInt

-- | Representation of deck options.
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


-- | Volatile fields of col.decks.
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


-- | Type for deck ids.
type DeckId = WeaklyTypedInt

-- | Representation of a deck.
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
