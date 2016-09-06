 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE RecordWildCards   #-}

module Anki.Tools (
    getUserProfiles
  , getCollections
  , getNotes
  ) where

import Anki.Misc
import Anki.Types
import Control.Exception (bracket)
import Database.SQLite.Simple (open, close, query_)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)


getUserProfiles :: AnkiPathsConfiguration -> IO [UserProfile]
getUserProfiles (AnkiPathsConfiguration {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, apcPrefs]) <$> getHomeDirectory))
  (close)
  (flip query_ "select name from profiles")

getCollections :: AnkiPathsConfiguration -> UserProfile -> IO [Collection]
getCollections (AnkiPathsConfiguration {..}) (UserProfile {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, upName, apcCollection]) <$> getHomeDirectory))
  (close)
  (flip query_ "select id, crt, mod, scm, ver, dty, usn, ls, conf, models, decks, dconf, tags from col")

getNotes :: AnkiPathsConfiguration -> UserProfile -> IO [Note]
getNotes (AnkiPathsConfiguration {..}) (UserProfile {..}) = bracket
  (open =<< (joinPath . (:[apcRoot, upName, apcCollection]) <$> getHomeDirectory))
  (close)
  (flip query_ "select id, guid, mid, mod, usn, tags, flds, sfld, csum, flags, data from notes")


