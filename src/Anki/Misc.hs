{-|
  Module:      Anki.Misc
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Misc types.
-}

module Anki.Misc (
    AnkiPathsConfiguration(..)
  ) where

import Data.Default (Default, def)

-- | Paths to all required files.
data AnkiPathsConfiguration = AnkiPathsConfiguration {
    apcRoot       :: FilePath -- ^ Root directory of Anki
  , apcPrefs      :: FilePath -- ^ Name of a preference file
  , apcCollection :: FilePath -- ^ Name of a collection file
  , apcMediaDB    :: FilePath -- ^ Name of a media database file
  , apcMediaDir   :: FilePath -- ^ Name of a directory with media
  } deriving (Show, Eq)

instance Default AnkiPathsConfiguration where
  def = AnkiPathsConfiguration {
     apcRoot       = "Documents/Anki"
   , apcPrefs      = "prefs.db"
   , apcCollection = "collection.anki2"
   , apcMediaDB    = "collection.media.db2"
   , apcMediaDir   = "collection.media"
   }
