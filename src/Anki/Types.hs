{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Anki.Types (
    UserProfile(..)
  , Collection(..)
  , Tag(..)
  ) where

import Control.Exception (Exception)
import Data.Aeson (Value(..), decode)
import Data.HashMap.Strict (toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (FromRow(..), SQLData(..), field)
import Database.SQLite.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Text as T


-- | The exception is thrown when something goes wrong with this package.
data AnkiException
  = TagsWrongFieldType
  -- ^ Thrown when col.tags is not a text value.
  | TagsNotJson
  -- ^ Thrown when text in col.tags is not a valid json.
  | TagsWrongJsonFormat
  -- ^ Thrown when col.tags format differs from expected
  deriving (Eq, Show, Typeable)

instance (Exception AnkiException)


-- | User profile as in profile table in prefs.db
data UserProfile = UserProfile {
    upName :: String -- ^ Profile name (profile.name)

  -- TODO: upData :: String -- ^ profile.data
  } deriving (Show, Eq)

instance FromRow UserProfile where
  fromRow = UserProfile <$> field

-- | Collection as in col table
data Collection = Collection {
    collectionId            :: Int            -- ^ collection identifier (id)
  , collectionCrt           :: Int            -- ^ TODO: wtf?
  , collectionMod           :: Int            -- ^ TODO: wtf?
  , collectionScm           :: Int            -- ^ TODO: wtf?
  , collectionVer           :: Int            -- ^ TODO: wtf?
  , collectionDty           :: Int            -- ^ TODO: wtf?
  , collectionUsn           :: Int            -- ^ TODO: wtf?
  , collectionLs            :: Int            -- ^ TODO: wtf?
  , collectionGlobalOptions :: GlobalOptions  -- ^ global options (col.conf)
  , collectionModels        :: [Model]        -- ^ models in the collection(col.models)
  , collectionDecks         :: [Deck]         -- ^ decks in the collection (col.decks)
  , collectionDeckOptions   :: [DeckOptions]  -- ^ deck options (col.dconf)
  , collectionTags          :: [Tag]          -- ^ tags (col.tags)
  } deriving (Show, Eq)

instance FromRow Collection where
  fromRow = Collection
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
    <*> field
    <*> field


throwErr :: (Typeable a) => Field -> AnkiException -> Ok a
throwErr f ex = returnError ConversionFailed f $ show ex


getJsonValue :: Field -> Ok Value
getJsonValue f = getText f >>= getValue where
  getText :: Field -> Ok Text
  getText = \case
    (Field (SQLText txt) _) -> return txt
    _                       -> throwErr f TagsWrongFieldType

  getValue :: Text -> Ok Value
  getValue = maybe (throwErr f TagsNotJson) return . (decode . BSLC8.pack . T.unpack)


-- | TODO: Global opitions
data GlobalOptions = GlobalOptions {

  } deriving (Show, Eq)

instance FromField GlobalOptions where
  fromField _f = return $ GlobalOptions {} -- TODO


-- | TODO: Model
data Model = Model {

  } deriving (Show, Eq)

instance FromField [Model] where
  fromField _f = return [] -- TODO


-- | TODO: Deck
data Deck = Deck {

  } deriving (Show, Eq)

instance FromField [Deck] where
  fromField _f = return [] -- TODO


-- | TODO: DeckOptions
data DeckOptions = DeckOptions {

  } deriving (Show, Eq)
instance FromField [DeckOptions] where
  fromField _f = return [] -- TODO


-- | Tags from col.tags
data Tag = Tag {
    tagName   :: String -- ^ tag name (key)
  , tagNumber :: Int    -- ^ TODO: wtf? (value)
  } deriving (Show, Eq)

instance FromField [Tag] where
  fromField f = getJsonValue f >>= mkTags where

    mkTags :: Value -> Ok [Tag]
    mkTags = \case
      (Object o) -> mapM mkTag (toList o)
      _          -> throwErr f TagsWrongJsonFormat

    mkTag :: (Text, Value) -> Ok Tag
    mkTag = \case
      (name, Number number) -> return $ Tag (T.unpack name) (round number)
      _                     -> throwErr f TagsWrongJsonFormat
