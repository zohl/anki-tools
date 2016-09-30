{-|
  Module:      Anki.Tools
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Tools to work with collection.
-}

 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE RecordWildCards   #-}

module Anki.Tools (
    getUserProfiles
  , getCollections
  , getNotes
  , getCards
  ) where

import Anki.Misc
import Anki.UserProfile
import Anki.Collection
import Anki.Note
import Anki.Card
import Control.Exception (bracket)
import Database.SQLite.Simple (open, close, query_)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)


-- | Extract user profiles from the given anki configuration.
getUserProfiles :: AnkiPathsConfiguration -> IO [UserProfile]
getUserProfiles (AnkiPathsConfiguration {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, apcPrefs]) <$> getHomeDirectory))
  (close)
  (flip query_ "select name from profiles")

-- | Extract available collections from the given profile.
getCollections :: AnkiPathsConfiguration -> UserProfile -> IO [Collection]
getCollections (AnkiPathsConfiguration {..}) (UserProfile {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, upName, apcCollection]) <$> getHomeDirectory))
  (close)
  (flip query_ "select id, crt, mod, scm, ver, dty, usn, ls, conf, models, decks, dconf, tags from col")

-- | Extract all notes from from the given profile.
getNotes :: AnkiPathsConfiguration -> UserProfile -> IO [Note]
getNotes (AnkiPathsConfiguration {..}) (UserProfile {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, upName, apcCollection]) <$> getHomeDirectory))
  (close)
  (flip query_ "select id, guid, mid, mod, usn, tags, flds, sfld, csum, flags, data from notes")

-- | Extract all cards from from the given profile.
getCards :: AnkiPathsConfiguration -> UserProfile -> IO [Card]
getCards (AnkiPathsConfiguration {..}) (UserProfile {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, upName, apcCollection]) <$> getHomeDirectory))
  (close)
  (flip query_ "select id, nid, did, ord, mod, usn, type, queue, due, ivl, factor, reps, lapses, left, odue, odid, flags, data from cards limit 20")
