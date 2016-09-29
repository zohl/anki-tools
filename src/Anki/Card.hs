{-|
  Module:      Anki.Card
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Representation of a card.
-}

{-# LANGUAGE DeriveGeneric #-}

module Anki.Card (
    CardId
  , Card(..)
  ) where


import Anki.Common (ModificationTime)
import Anki.Deck (DeckId)
import Anki.Note (NoteId)
import Data.Text (Text)
import Database.SQLite.Simple (FromRow(..), field)
import GHC.Generics (Generic)

-- | Type for card ids.
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
