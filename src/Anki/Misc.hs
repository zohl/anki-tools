
module Anki.Misc (
    AnkiPathsConfiguration(..)
  ) where

import Data.Default (Default, def)

data AnkiPathsConfiguration = AnkiPathsConfiguration {
    apcRoot       :: FilePath
  , apcPrefs      :: FilePath
  , apcCollection :: FilePath
  , apcMediaDB    :: FilePath
  , apcMediaDir   :: FilePath
  } deriving (Show, Eq)

instance Default AnkiPathsConfiguration where
  def = AnkiPathsConfiguration {
     apcRoot       = "Documents/Anki"
   , apcPrefs      = "prefs.db"
   , apcCollection = "collection.anki2"
   , apcMediaDB    = "collection.media.db2"
   , apcMediaDir   = "collection.media"
   }
