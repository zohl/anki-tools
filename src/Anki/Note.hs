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

