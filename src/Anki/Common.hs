{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Anki.Common (
    AnkiException(..)
  , WeaklyTypedInt(..)
  , WeaklyTypedBool(..)
  , ModificationTime(..)
  , throwErr
  , getTextValue
  , getJsonValue
  , fromDictionary
  , mkEntry
  , dropPrefixOptions
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Aeson (Value(..), encode, decode, FromJSON(..), genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions, (.:), withObject)
import Data.Char (toLower, isUpper, chr)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.HashMap.Strict (toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (FromRow(..), SQLData(..), field)
import Database.SQLite.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Text as T
import Debug.Trace

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

throwErr :: (Typeable a) => Field -> AnkiException -> Ok a
throwErr f ex = returnError ConversionFailed f $ show ex

getTextValue :: Field -> Ok BSLC8.ByteString
getTextValue = \case
  (Field (SQLText txt) _) -> return . BSLC8.pack . T.unpack $ txt
  f                       -> throwErr f WrongFieldType

getJsonValue :: Field -> Ok Value
getJsonValue f = getTextValue f >>= getValue where
  getValue :: BSLC8.ByteString -> Ok Value
  getValue = maybe (throwErr f NotJson) return . decode

fromDictionary :: (Typeable a) => (Field -> (Text, Value) -> Ok a) -> Field -> Value -> Ok [a]
fromDictionary mkEntry' f = \case
  (Object o) -> mapM (mkEntry' f) (toList o)
  _          -> throwErr f WrongJsonFormat


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


dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix }


newtype WeaklyTypedInt = WeaklyTypedInt { getInt :: Int } deriving (Show, Eq, Num)

instance FromJSON WeaklyTypedInt where
  parseJSON = fmap fromInteger . \case
    (String s) -> return . read . T.unpack $ s
    (Number x) -> return . round $ x
    _ -> error "TODO"

instance FromField WeaklyTypedInt where
  fromField f = fromInteger <$> fromField f


newtype WeaklyTypedBool = WeaklyTypedBool { getBool :: Bool } deriving (Show, Eq)

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


newtype ModificationTime = ModificationTime { getModificationTime :: UTCTime } deriving (Show, Eq)

instance FromField ModificationTime where
  fromField f = (ModificationTime . posixSecondsToUTCTime . fromInteger) <$> fromField f

instance FromJSON ModificationTime where
  parseJSON = fmap (ModificationTime . posixSecondsToUTCTime . fromInteger) . parseJSON

