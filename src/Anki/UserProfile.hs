{-|
  Module:      Anki.UserProfile
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Representation of a user profile and related definitions.
-}

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

