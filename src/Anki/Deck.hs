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
import Anki.Common (TimeIntervalInSeconds(..), TimeIntervalInMinutes(..), TimeIntervalInDays(..))
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


-- | Options from cols.deck.dconf.new.
data DeckOptionsNew = DeckOptionsNew {
    donPerDay        :: Int                     -- ^ Number of new cards a day.
  , donDelays        :: [TimeIntervalInMinutes] -- ^ Duration between checks of a new card.
  , donSeparate      :: Bool                    -- ^ (not used)
  , donInts          :: [TimeIntervalInDays]    -- ^ Duration between checks of a regular card.
  , donInitialFactor :: Double                  -- ^ Starting ease (in permilles)
  , donBury          :: Bool                    -- ^ Bury related cards until the next day.
  , donOrder         :: WeaklyTypedBool         -- ^ Don't shuffle cards
  } deriving (Show, Eq, Generic)

instance Default DeckOptionsNew where
  def = DeckOptionsNew {
    donPerDay        = 20
  , donDelays        = TimeIntervalInMinutes <$> [1, 10]
  , donSeparate      = True
  , donInts          = TimeIntervalInDays <$> [1, 4, 7]
  , donInitialFactor = 2500.0
  , donBury          = True
  , donOrder         = WeaklyTypedBool True
  }

instance FromJSON DeckOptionsNew where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.lapse.
data DeckOptionsLapse = DeckOptionsLapse {
    dolLeechFails  :: Int                     -- ^ Leech threshold.
  , dolMinInt      :: TimeIntervalInDays      -- ^ Minimal interval.
  , dolDelays      :: [TimeIntervalInMinutes] -- ^ Duration between checks of a lapsed card.
  , dolLeechAction :: WeaklyTypedBool         -- ^ Don't suspend card after reaching the threshold.
  , dolMult        :: Double                  -- ^ Reduce check interval by given multiplier.
  } deriving (Show, Eq, Generic)

instance Default DeckOptionsLapse where
  def = DeckOptionsLapse {
    dolLeechFails  = 8
  , dolMinInt      = TimeIntervalInDays 1
  , dolDelays      = TimeIntervalInMinutes <$> [10]
  , dolLeechAction = WeaklyTypedBool False
  , dolMult        = 0.0
  }

instance FromJSON DeckOptionsLapse where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.rev
data DeckOptionsRev = DeckOptionsRev {
    dorPerDay   :: Int                -- ^ Numbers of maximum cards to review each day.
  , dorFuzz     :: Double             -- ^ (not used)
  , dorIvlFct   :: Double             -- ^ Increase check interval by the given multiplier.
  , dorMaxIvl   :: TimeIntervalInDays -- ^ Maximum interval.
  , dorEase4    :: Double             -- ^ Increase check interval by the given
                                      --   multiplier when "easy" button is pressed.
  , dorBury     :: Bool               -- ^ Bury related cards to the next day.
  , dorMinSpace :: Maybe Value        -- ^ (not used)
  } deriving (Show, Eq, Generic)

instance Default DeckOptionsRev where
  def = DeckOptionsRev {
    dorPerDay   = 100
  , dorFuzz     = 0.05
  , dorIvlFct   = 1
  , dorMaxIvl   = TimeIntervalInDays 36500
  , dorEase4    = 1.3
  , dorBury     = True
  , dorMinSpace = Just (Number 1.0)
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
