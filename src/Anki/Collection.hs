{-|
  Module:      Anki.Collection
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Representation of a collection and related definitions.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Anki.Collection (
    Collection(..)
  , GlobalOptions(..)
  , Tag(..)
  ) where


import Anki.Common (ModificationTime, TimeIntervalInMinutes(..), AnkiException(..), throwErr)
import Anki.Common (dropPrefixOptions, getTextValue, getJsonValue, fromDictionary)
import Anki.Deck (DeckId, Deck, DeckOptions)
import Anki.Card (CardId)
import Anki.Model (ModelId, Model)
import Data.Aeson (Value(..), decode, FromJSON(..), genericParseJSON)
import Data.Default(Default(..))
import Data.Text (Text)
import Database.SQLite.Simple (FromRow(..), field)
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import GHC.Generics (Generic)
import qualified Data.Text as T


-- | Collection as in col table.
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


-- | Global opitions.
data GlobalOptions = GlobalOptions {
   goNextPos       :: Int                   -- ^ New card position.
 , goEstTimes      :: Bool                  -- ^ Do show estimates.
 , goSortBackwards :: Bool                  -- ^ Reverse order in cards browser.
 , goSortType      :: String                -- ^ How to sort cards in the browser.
 , goTimeLim       :: TimeIntervalInMinutes -- ^ Timebox time limit.
 , goActiveDecks   :: [DeckId]              -- ^ Currently active decks.
 , goAddToCur      :: Bool                  -- ^ Unconditionally add cards to current deck.
 , goCurDeck       :: DeckId                -- ^ Current deck.
 , goCurModel      :: Maybe ModelId         -- ^ Current model.
 , goLastUnburied  :: Maybe CardId          -- ^ The last unburied card.
 , goCollapseTime  :: Maybe Value           -- TODO wtf?
 , goActiveCols    :: Maybe Value           -- TODO [String]?
 , goSavedFilters  :: Maybe Value           -- TODO [wtf]?
 , goDueCounts     :: Bool                  -- ^ Show checked cards in progress.
 , goNewBury       :: Maybe Value           -- TODO Bool? not used?
 , goNewSpread     :: Int                   -- ^ How to combine new and old cards.
 } deriving (Show, Eq, Generic)

instance Default GlobalOptions where
  def = GlobalOptions {
      goNextPos       = 1
    , goEstTimes      = True
    , goSortBackwards = False
    , goSortType      = "noteFld"
    , goTimeLim       = TimeIntervalInMinutes 0
    , goActiveDecks   = [1]
    , goAddToCur      = True
    , goCurDeck       = 1
    , goCurModel      = Nothing
    , goLastUnburied  = Nothing
    , goCollapseTime  = undefined -- TODO 1200.0
    , goActiveCols    = undefined -- TODO Nothing
    , goSavedFilters  = undefined -- Nothing
    , goDueCounts     = True
    , goNewBury       = undefined -- TODO True
    , goNewSpread     = 0
    }

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

