{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Anki.Types (
    UserProfile(..)
  , Collection(..)
  , GlobalOptions(..)

  , WeaklyTypedInt(..)
  , ModelId
  , DeckId
  , DeckOptionsId

  , Model(..)
  , ModelField(..)
  , ModelTemplate(..)

  , Deck(..)
  , DeckOptions(..)
  , DeckOptionsLapse(..)
  , DeckOptionsRev(..)
  , DeckOptionsNew(..)

  , Tag(..)
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Aeson (Value(..), encode, decode, FromJSON(..), genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions, (.:), withObject)
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
import Data.Char (toLower, isUpper)

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
  , collectionCrt           :: Int            -- TODO check type
  , collectionMod           :: Int            -- TODO check type
  , collectionScm           :: Int            -- TODO check type
  , collectionVer           :: Int            -- TODO check type
  , collectionDty           :: Int            -- TODO check type
  , collectionUsn           :: Int            -- TODO check type
  , collectionLs            :: Int            -- TODO check type
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

getTextValue :: Field -> Ok BSLC8.ByteString
getTextValue = \case
  (Field (SQLText txt) _) -> return . BSLC8.pack . T.unpack $ txt
  f                       -> throwErr f WrongFieldType

getJsonValue :: Field -> Ok Value
getJsonValue f = getTextValue f >>= getValue where
  getValue :: BSLC8.ByteString -> Ok Value
  getValue = maybe (throwErr f NotJson) return . decode


fromDictionary :: (Typeable a) => (Field -> (Text, Value) -> Ok a) -> Field -> Value -> Ok [a]
fromDictionary mkEntry f = \case
  (Object o) -> mapM (mkEntry f) (toList o)
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


newtype WeaklyTypedInt = WeaklyTypedInt { getInt :: Int } deriving (Show, Eq)
-- TODO Num WeaklyTypedInt

type DeckId = Int
type ModelId = WeaklyTypedInt
type DeckOptionsId = WeaklyTypedInt

instance FromJSON ModelId where
  parseJSON (String s) = return . WeaklyTypedInt . read . T.unpack $ s
  parseJSON (Number x) = return . WeaklyTypedInt . round $ x
  parseJSON _ = undefined

-- | Global opitions
data GlobalOptions = GlobalOptions {
   goNextPos       :: Value     -- TODO Int?
 , goEstTimes      :: Value     -- TODO Bool?
 , goSortBackwards :: Value     -- TODO Bool?
 , goSortType      :: Value     -- TODO String?
 , goTimeLim       :: Value     -- TODO Int?
 , goActiveDecks   :: [DeckId]  -- ^ TODO
 , goAddToCur      :: Value     -- TODO Bool?
 , goCurDeck       :: Value     -- TODO DeckId?
 , goCurModel      :: ModelId   -- ^ TODO
 , goLastUnburied  :: Value     -- TODO Int?
 , goCollapseTime  :: Value     -- TODO Int?
 , goActiveCols    :: Value     -- TODO [String]?
 , goSavedFilters  :: Value     -- TODO [wtf]?
 , goDueCounts     :: Value     -- TODO Bool?
 , goNewBury       :: Value     -- TODO Bool?
 , goNewSpread     :: Value     -- TODO Int?
 } deriving (Show, Eq, Generic)

instance FromJSON GlobalOptions where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField GlobalOptions where
  fromField f = getTextValue f >>= maybe (throwErr f WrongJsonFormat) return . decode


-- | Model from col.models
data Model = Model {
    modelId        :: ModelId
  , modelCss       :: Value -- String?
  , modelDid       :: Value -- DeckId?
  , modelFlds      :: [ModelField]
  , modelLatexPre  :: Value -- TODO String?
  , modelLatexPost :: Value -- TODO String?
  , modelMod       :: Value -- TODO Int?
  , modelName      :: Value -- TODO String?
  , modelSortf     :: Value -- TODO Int?
  , modelTags      :: Value -- TODO: [wtf]?
  , modelTmpls     :: [ModelTemplate]
  , modelType      :: Value -- TODO Int?
  , modelUsn       :: Value -- TODO Int?
  , modelVers      :: Value -- TODO [wtf]?
  } deriving (Show, Eq, Generic)

instance FromJSON Model where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField [Model] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry modelId ModelIdInconsistent) f where


-- | Field from col.models.flds
data ModelField = ModelField {
    mfName   :: String
  , mfMedia  :: Value -- TODO [wtf]?
  , mfSticky :: Value -- TODO Bool?
  , mfRtl    :: Value -- TODO Bool?
  , mfOrd    :: Value -- TODO Int?
  , mfFont   :: Value -- TODO String?
  , mfSize   :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON ModelField where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Template from col.models.tmpls
data ModelTemplate = ModelTemplate {
    mtName  :: String
  , mtQfmt  :: Value -- TODO String?
  , mtDid   :: Value -- TODO DeckId?
  , mtBafmt :: Value -- TODO String?
  , mtAfmt  :: Value -- TODO String?
  , mtBqfmt :: Value -- TODO String?
  , mtOrd   :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON ModelTemplate where
  parseJSON = genericParseJSON dropPrefixOptions


-- | TODO: Deck
data Deck = Deck {
    deckName              :: Value         -- TODO String?
  , deckCollapsed         :: Value         -- TODO Bool?
  , deckDesc              :: Value         -- TODO String?
  , deckId                :: Value         -- TODO DeckId?
  , deckMod               :: Value         -- TODO Int?
  , deckUsn               :: Value         -- TODO Int?
  , deckLrnToday          :: Value         -- TODO (Int, Int)?
  , deckNewToday          :: Value         -- TODO (Int, Int)?
  , deckRevToday          :: Value         -- TODO (Int, Int)?
  , deckTimeToday         :: Value         -- TODO (Int, Int)?
  , deckExtension         :: DeckExtension
  } deriving (Show, Eq, Generic)


instance FromJSON Deck where
  parseJSON = withObject "Deck" $ \o -> do
    deckName      <- o .: "name"
    deckCollapsed <- o .: "collapsed"
    deckDesc      <- o .: "desc"
    deckId        <- o .: "id"
    deckMod       <- o .: "mod"
    deckUsn       <- o .: "usn"
    deckLrnToday  <- o .: "lrnToday"
    deckNewToday  <- o .: "newToday"
    deckRevToday  <- o .: "revToday"
    deckTimeToday <- o .: "timeToday"
    deckExtension <- parseJSON $ Object o
    return Deck {..}

instance FromField [Deck] where
  fromField f = getJsonValue f >>= fromDictionary (mkEntry deckId DeckIdInconsistent) f


-- | Volatile fields of col.decks
data DeckExtension
  = NormalDeck {
      deckBrowserCollapsed  :: Value   -- TODO Bool?
    , deckConf              :: Value   -- TODO DeckOptionsId?
    , deckExtendNew         :: Value   -- TODO Int?
    , deckExtendRev         :: Value   -- TODO Int?
    }
  | DynamicDeck {
      deckDelays            :: Value   -- TODO Int?
    , deckResched           :: Value   -- TODO Bool?
    , deckReturn            :: Value   -- TODO Bool?
    , deckSeparate          :: Value   -- TODO Bool?
    , deckTerms             :: Value   -- TODO [[wtf]]?
    } deriving (Show, Eq, Generic)

instance FromJSON DeckExtension where
  parseJSON = withObject "DeckExtension" $ \o -> (/= (0 :: Int)) <$> (o .: "dyn") >>= \case
      False -> do
        deckBrowserCollapsed <- o .: "browserCollapsed"
        deckConf             <- o .: "conf"
        deckExtendNew        <- o .: "extendNew"
        deckExtendRev        <- o .: "extendRev"
        return NormalDeck {..}

      True -> do
        deckDelays   <- o .: "delays"
        deckResched  <- o .: "resched"
        deckReturn   <- o .: "return"
        deckSeparate <- o .: "separate"
        deckTerms    <- o .: "terms"
        return DynamicDeck {..}


-- | TODO: DeckOptions
data DeckOptions = DeckOptions {
    doId       :: DeckOptionsId
  , doAutoplay :: Value -- TODO Bool?
  , doDyn      :: Value -- TODO Bool?
  , doLapse    :: DeckOptionsLapse  -- ^ TODO
  , doMaxTaken :: Value -- TODO Int?
  , doMod      :: Value -- TODO Int?
  , doName     :: Value -- TODO String?
  , doNew      :: DeckOptionsNew    -- ^ TODO
  , doReplayq  :: Value -- TODO Bool?
  , doRev      :: DeckOptionsRev    -- ^ TODO
  , doTimer    :: Value -- TODO Int/Bool?
  , doUsn      :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptions where
  parseJSON = genericParseJSON dropPrefixOptions

instance FromField [DeckOptions] where
  fromField f = getJsonValue f >>= mkDeckOptions where

    mkDeckOptions :: Value -> Ok [DeckOptions]
    mkDeckOptions = \case
      (Object o) -> mapM mkDeckOption (toList o)
      _          -> throwErr f WrongJsonFormat

    mkDeckOption :: (Text, Value) -> Ok DeckOptions
    mkDeckOption = \case
      (key, value) -> do
        doId' <- maybe (throwErr f WrongJsonFormat) return . decode . BSLC8.pack . T.unpack $ key
        deckOption <- maybe (throwErr f WrongJsonFormat) return . decode . encode $ value
        unless (doId' == doId deckOption) $ throwErr f DeckOptionsIdInconsistent
        return deckOption

-- | Options from cols.deck.dconf.lapse
data DeckOptionsLapse = DeckOptionsLapse {
    dolLeechFails  :: Value -- TODO Int?
  , dolMinInt      :: Value -- TODO Int?
  , dolDelays      :: Value -- TODO [Int]?
  , dolLeechAction :: Value -- TODO Int/Enum?
  , dolMult        :: Value -- TODO Double?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptionsLapse where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.new
data DeckOptionsNew = DeckOptionsNew {
    donPerDay        :: Value -- TODO Int?
  , donDelays        :: Value -- TODO (Int, Int)?
  , donSeparate      :: Value -- TODO Bool?
  , donInts          :: Value -- TODO (Int, Int, Int)?
  , donInitialFactor :: Value -- TODO Double?
  , donBury          :: Value -- TODO Bool?
  , donOrder         :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptionsNew where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Options from cols.deck.dconf.rev
data DeckOptionsRev = DeckOptionsRev {
    dorPerDay   :: Value -- TODO Int?
  , dorFuzz     :: Value -- TODO Double?
  , dorIvlFct   :: Value -- TODO Double?
  , dorMaxIvl   :: Value -- TODO Double?
  , dorEase4    :: Value -- TODO Double?
  , dorBury     :: Value -- TODO Bool?
  , dorMinSpace :: Value -- TODO Int?
  } deriving (Show, Eq, Generic)

instance FromJSON DeckOptionsRev where
  parseJSON = genericParseJSON dropPrefixOptions


-- | Tags from col.tags
data Tag = Tag {
    tagName   :: String -- ^ tag name (key)
  , tagNumber :: Int    -- ^ TODO: wtf? (value)
  } deriving (Show, Eq)

instance FromField [Tag] where
  fromField f = getJsonValue f >>= fromDictionary mkTag f where
    mkTag :: Field -> (Text, Value) -> Ok Tag
    mkTag f' = \case
      (name, Number number) -> return $ Tag (T.unpack name) (round number)
      _                     -> throwErr f' WrongJsonFormat
