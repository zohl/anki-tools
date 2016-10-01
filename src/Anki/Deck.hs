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


import Anki.Common (WeaklyTypedInt(..), WeaklyTypedBool(..), ModificationTime(..), AnkiException(..))
import Anki.Common (TimeIntervalInSeconds(..))
import Anki.Common (dropPrefixOptions, getJsonValue, fromDictionary, mkEntry)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON)
import Data.Aeson.Types ((.:), (.:?), withObject)
import Data.Default (Default(..))
import Database.SQLite.Simple.FromField (FromField(..))
import GHC.Generics (Generic)

-- | Type for deck options ids.
type DeckOptionsId = WeaklyTypedInt

-- | Representation of deck options.
data DeckOptions = DeckOptions {
    doId       :: DeckOptionsId         -- ^ Deck options id.
  , doAutoplay :: Bool                  -- ^ Do play media automatically?
  , doDyn      :: Bool                  -- ^ Is deck dynamical?
  , doMaxTaken :: TimeIntervalInSeconds -- ^ Ignore answers after given time interval.
  , doName     :: String                -- ^ Name of the options group.
  , doReplayq  :: Bool                  -- ^ Do replay media from question when showing answer?
  , doTimer    :: WeaklyTypedBool       -- ^ Do show timer?
  , doMod      :: ModificationTime      -- ^ Modification time
  , doUsn      :: Maybe Value           -- TODO Int?
  , doNew      :: DeckOptionsNew        -- ^ Options for new cards.
  , doLapse    :: DeckOptionsLapse      -- ^ Options for lapses.
  , doRev      :: DeckOptionsRev        -- ^ Options for reviews.
  } deriving (Show, Eq, Generic)

instance Default DeckOptions where
  def = DeckOptions {
      doId       = WeaklyTypedInt 0
    , doAutoplay = True
    , doDyn      = False
    , doMaxTaken = TimeIntervalInSeconds 60
    , doName     = "Default"
    , doReplayq  = True
    , doTimer    = WeaklyTypedBool False
    , doMod      = def
    , doUsn      = undefined -- TODO
    , doNew      = def
    , doLapse    = def
    , doRev      = def
    }

instance FromJSON DeckOptions where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField [DeckOptions] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry doId DeckOptionsIdInconsistent) f


-- | Options from cols.deck.dconf.lapse.
data DeckOptionsLapse = DeckOptionsLapse {
    dolLeechFails  :: Value -- TODO Int?
  , dolMinInt      :: Value -- TODO Int?
  , dolDelays      :: Value -- TODO [Int]?
  , dolLeechAction :: Value -- TODO Int/Enum?
  , dolMult        :: Value -- TODO Double?
  } deriving (Show, Eq, Generic)

instance Default DeckOptionsLapse where
  def = DeckOptionsLapse {
    dolLeechFails  = undefined -- TODO
  , dolMinInt      = undefined -- TODO
  , dolDelays      = undefined -- TODO
  , dolLeechAction = undefined -- TODO
  , dolMult        = undefined -- TODO
  }

instance FromJSON DeckOptionsLapse where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.new.
data DeckOptionsNew = DeckOptionsNew {
    donPerDay        :: Maybe Value -- TODO Int?
  , donDelays        :: Maybe Value -- TODO (Int, Int)?
  , donSeparate      :: Maybe Value -- TODO Bool?
  , donInts          :: Maybe Value -- TODO (Int, Int, Int)?
  , donInitialFactor :: Maybe Value -- TODO Double?
  , donBury          :: Maybe Value -- TODO Bool?
  , donOrder         :: Maybe Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance Default DeckOptionsNew where
  def = DeckOptionsNew {
    donPerDay        = undefined -- TODO
  , donDelays        = undefined -- TODO
  , donSeparate      = undefined -- TODO
  , donInts          = undefined -- TODO
  , donInitialFactor = undefined -- TODO
  , donBury          = undefined -- TODO
  , donOrder         = undefined -- TODO
  }

instance FromJSON DeckOptionsNew where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.rev
data DeckOptionsRev = DeckOptionsRev {
    dorPerDay   :: Maybe Value -- TODO Int?
  , dorFuzz     :: Maybe Value -- TODO Double?
  , dorIvlFct   :: Maybe Value -- TODO Double?
  , dorMaxIvl   :: Maybe Value -- TODO Double?
  , dorEase4    :: Maybe Value -- TODO Double?
  , dorBury     :: Maybe Value -- TODO Bool?
  , dorMinSpace :: Maybe Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance Default DeckOptionsRev where
  def = DeckOptionsRev {
    dorPerDay   = undefined -- TODO
  , dorFuzz     = undefined -- TODO
  , dorIvlFct   = undefined -- TODO
  , dorMaxIvl   = undefined -- TODO
  , dorEase4    = undefined -- TODO
  , dorBury     = undefined -- TODO
  , dorMinSpace = undefined -- TODO
  }

instance FromJSON DeckOptionsRev where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Type for deck ids.
type DeckId = WeaklyTypedInt

-- | Volatile fields of col.decks.
data DeckExtension
  = NormalDeck {
      deckBrowserCollapsed  :: Maybe Value   -- TODO Bool?
    , deckConf              :: Maybe Value   -- TODO DeckOptionsId?
    , deckExtendNew         :: Maybe Value   -- TODO Int?
    , deckExtendRev         :: Maybe Value   -- TODO Int?
    }
  | DynamicDeck {
      deckDelays            :: Maybe Value   -- TODO Int?
    , deckResched           :: Maybe Value   -- TODO Bool?
    , deckReturn            :: Maybe Value   -- TODO Bool?
    , deckSeparate          :: Maybe Value   -- TODO Bool?
    , deckTerms             :: Maybe Value   -- TODO [[wtf]]?
    } deriving (Show, Eq, Generic)

instance FromJSON DeckExtension where
  parseJSON = withObject "DeckExtension" $ \o -> getBool <$> (o .: "dyn") >>= \case
      False -> do
        deckBrowserCollapsed <- o .:? "browserCollapsed"
        deckConf             <- o .:? "conf"
        deckExtendNew        <- o .:? "extendNew"
        deckExtendRev        <- o .:? "extendRev"
        return NormalDeck {..}

      True -> do
        deckDelays   <- o .:? "delays"
        deckResched  <- o .:? "resched"
        deckReturn   <- o .:? "return"
        deckSeparate <- o .:? "separate"
        deckTerms    <- o .:? "terms"
        return DynamicDeck {..}

-- | Representation of a deck (col.decks).
data Deck = Deck {
    deckId                :: DeckId
  , deckName              :: Maybe Value         -- TODO String?
  , deckCollapsed         :: Maybe Value         -- TODO Bool?
  , deckDesc              :: Maybe Value         -- TODO String?
  , deckMod               :: ModificationTime
  , deckUsn               :: Maybe Value         -- TODO Int?
  , deckLrnToday          :: Maybe Value         -- TODO (Int, Int)?
  , deckNewToday          :: Maybe Value         -- TODO (Int, Int)?
  , deckRevToday          :: Maybe Value         -- TODO (Int, Int)?
  , deckTimeToday         :: Maybe Value         -- TODO (Int, Int)?
  , deckExtension         :: DeckExtension
  } deriving (Show, Eq, Generic)


instance FromJSON Deck where
  parseJSON = withObject "Deck" $ \o -> do
    deckName      <- o .:? "name"
    deckCollapsed <- o .:? "collapsed"
    deckDesc      <- o .:? "desc"
    deckId        <- o .:  "id"
    deckMod       <- o .:  "mod"
    deckUsn       <- o .:? "usn"
    deckLrnToday  <- o .:? "lrnToday"
    deckNewToday  <- o .:? "newToday"
    deckRevToday  <- o .:? "revToday"
    deckTimeToday <- o .:? "timeToday"
    deckExtension <- parseJSON $ Object o
    return Deck {..}

instance FromField [Deck] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry deckId DeckIdInconsistent) f
