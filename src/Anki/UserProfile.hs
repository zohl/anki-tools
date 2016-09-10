module Anki.UserProfile (
    UserProfile(..)
  ) where

import Database.SQLite.Simple (FromRow(..), field)

-- | User profile as in profile table in prefs.db
data UserProfile = UserProfile {
    upName :: String -- ^ Profile name (profile.name)

  -- TODO: upData :: String -- ^ profile.data
  } deriving (Show, Eq)

instance FromRow UserProfile where
  fromRow = UserProfile <$> field

