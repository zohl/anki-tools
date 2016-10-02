{-|
  Module:      Anki.Common
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Auxiliary functions and types.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Anki.Common (
    AnkiException(..)
  , WeaklyTypedInt(..)
  , WeaklyTypedBool(..)
  , ModificationTime(..)
  , TimeIntervalInSeconds(..)
  , TimeIntervalInMinutes(..)
  , TimeIntervalInDays(..)
  , throwErr
  , getTextValue
  , getJsonValue
  , fromDictionary
  , mkEntry
  , dropPrefixOptions
  ) where


import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Aeson (Value(..), encode, decode, FromJSON(..))
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Char (toLower, isUpper)
import Data.Default (Default(..))
import Data.HashMap.Strict (toList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (SQLData(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Text as T


-- | The exception is thrown when something goes wrong with this package.
data AnkiException
  = WrongFieldType
  -- ^ Thrown when column type is not a text.
  | NotJson
  -- ^ Thrown when text from database is not a valid json.
  | WrongJsonFormat
  -- ^ Thrown when json format differs from expected one.
  | ModelIdInconsistent
  -- ^ Thrown when external and internal ids of model differ.
  | DeckIdInconsistent
  -- ^ Thrown when external and internal ids of deck differ.
  | DeckOptionsIdInconsistent
  -- ^ Thrown when external and internal ids of deck options differ.
  deriving (Eq, Show, Typeable)

instance (Exception AnkiException)

-- | Exit from Ok monad.
throwErr :: (Typeable a) => Field -> AnkiException -> Ok a
throwErr f ex = returnError ConversionFailed f $ show ex

-- | Read field as a byte sequence.
getTextValue :: Field -> Ok BSLC8.ByteString
getTextValue = \case
  (Field (SQLText txt) _) -> return . BSLC8.pack . T.unpack $ txt
  f                       -> throwErr f WrongFieldType

-- | Read field as a JSON.
getJsonValue :: Field -> Ok Value
getJsonValue f = getTextValue f >>= getValue where
  getValue :: BSLC8.ByteString -> Ok Value
  getValue = maybe (throwErr f NotJson) return . decode

-- | Transform a JSON-dictionary to a list of values.
fromDictionary :: (Typeable a) => (Field -> (Text, Value) -> Ok a) -> Field -> Value -> Ok [a]
fromDictionary mkEntry' f = \case
  (Object o) -> mapM (mkEntry' f) (toList o)
  _          -> throwErr f WrongJsonFormat

-- | Transform a single pair from JSON-dictionary of type { <id>: {id: <id>, ....} } to a record.
mkEntry :: (Typeable a, FromJSON a, Eq b, Typeable b, FromJSON b)
  => (a -> b)
  -> AnkiException
  -> Field
  -> (Text, Value)
  -> Ok a

mkEntry entryId entryIdException f (key, value) = do
  entryId' <- maybe
    (throwErr f WrongJsonFormat)
    return
    (decode . BSLC8.pack . T.unpack $ key)

  entry <- maybe
    (throwErr f WrongJsonFormat)
    return
    (decode . encode $ value)

  unless (entryId' == entryId entry) $ throwErr f entryIdException
  return entry

-- | Cut the first word and lowercase the second.
dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

-- | Default options used in Aeson typeclasses in this module.
dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix }

-- | A wrapper to handle integers and strings with integers.
newtype WeaklyTypedInt = WeaklyTypedInt { getInt :: Int } deriving (Eq, Num)

instance Show WeaklyTypedInt where show = show . getInt

instance FromJSON WeaklyTypedInt where
  parseJSON = fmap fromInteger . \case
    (String s) -> return . read . T.unpack $ s
    (Number x) -> return . round $ x
    _ -> error "TODO"

instance FromField WeaklyTypedInt where
  fromField f = fromInteger <$> fromField f


-- | A wrapper to handle booleans, strings with booleans and 0-1 integers.
newtype WeaklyTypedBool = WeaklyTypedBool { getBool :: Bool } deriving (Eq)

instance Show WeaklyTypedBool where show = show . getBool

instance FromJSON WeaklyTypedBool where
  parseJSON = fmap WeaklyTypedBool . \case
    (String s) -> case s of
      "false" -> return False
      "true"  -> return True
      _       -> error "TODO"

    (Number x) -> case x of
      0 -> return False
      1 -> return True
      _ -> error "TODO"

    _ -> error "TODO"


-- | A wrapper handle time in POSIX format.
newtype ModificationTime = ModificationTime { getModificationTime :: UTCTime } deriving (Eq)

instance Show ModificationTime where show = show . getModificationTime

instance Default ModificationTime where
  def = ModificationTime . posixSecondsToUTCTime . fromInteger $ 0

instance FromField ModificationTime where
  fromField f = (ModificationTime . posixSecondsToUTCTime . fromInteger) <$> fromField f

instance FromJSON ModificationTime where
  parseJSON = fmap (ModificationTime . posixSecondsToUTCTime . fromInteger) . parseJSON



-- | A wrapper for time interval (in seconds).
newtype TimeIntervalInSeconds = TimeIntervalInSeconds { getTimeIntervalInSeconds :: NominalDiffTime }
  deriving (Eq)

instance Show TimeIntervalInSeconds where show = show . getTimeIntervalInSeconds

instance Default TimeIntervalInSeconds where
  def = TimeIntervalInSeconds . fromInteger $ 0

instance FromField TimeIntervalInSeconds where
  fromField f = (TimeIntervalInSeconds . fromInteger) <$> fromField f

instance FromJSON TimeIntervalInSeconds where
  parseJSON = fmap (TimeIntervalInSeconds . fromInteger) . parseJSON


-- | A wrapper for time interval (in minutes).
newtype TimeIntervalInMinutes = TimeIntervalInMinutes { getTimeIntervalInMinutes :: NominalDiffTime }
  deriving (Eq)

instance Show TimeIntervalInMinutes where show = show . getTimeIntervalInMinutes

instance Default TimeIntervalInMinutes where
  def = TimeIntervalInMinutes . fromInteger $ 0

instance FromField TimeIntervalInMinutes where
  fromField f = (TimeIntervalInMinutes . fromInteger . (flip div 60)) <$> fromField f

instance FromJSON TimeIntervalInMinutes where
  parseJSON = fmap (TimeIntervalInMinutes . fromInteger . (flip div 60)) . parseJSON


-- | A wrapper for time interval (in days).
newtype TimeIntervalInDays = TimeIntervalInDays { getTimeIntervalInDays :: NominalDiffTime }
  deriving (Eq)

instance Show TimeIntervalInDays where show = show . getTimeIntervalInDays

instance Default TimeIntervalInDays where
  def = TimeIntervalInDays . fromInteger $ 0

instance FromField TimeIntervalInDays where
  fromField f = (TimeIntervalInDays . fromInteger . (flip div (24*60*60))) <$> fromField f

instance FromJSON TimeIntervalInDays where
  parseJSON = fmap (TimeIntervalInDays . fromInteger . (flip div (24*60*60))) . parseJSON
