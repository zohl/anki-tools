{-|
  Module:      Anki.Note
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Representation of a note and related definitions.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Anki.Note (
    NoteId
  , Note(..)
  , NoteField(..)
  ) where

import Anki.Common (ModificationTime)
import Anki.Model (ModelId)
import Data.Char (chr)
import Data.Text (Text)
import Database.SQLite.Simple (FromRow(..), field)
import Database.SQLite.Simple.FromField (FromField(..))
import GHC.Generics (Generic)
import qualified Data.Text as T

-- | Type for note ids.
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

-- | Representation of note fields as they store in the database.
newtype NoteField = NoteField { getNoteField :: Text } deriving (Show, Eq)

instance FromField NoteField where
  fromField f = NoteField <$> fromField f

instance FromField [NoteField] where
  fromField f = (fmap NoteField . T.split (== fieldSeparator)) <$> fromField f

