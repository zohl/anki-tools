{-|
  Module:      Anki.Model
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Representation of a model and related definitions.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Anki.Model (
    ModelId
  , Model(..)
  , ModelField(..)
  , ModelTemplate(..)
  ) where

import Anki.Common (WeaklyTypedInt, ModificationTime, AnkiException(..)) 
import Anki.Common (dropPrefixOptions, getJsonValue, fromDictionary, mkEntry)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON)
import Data.Text (Text)
import Data.Default (Default(..))
import Database.SQLite.Simple.FromField (FromField(..))
import GHC.Generics (Generic)
import qualified Data.Text as T

-- | Type for model ids.
type ModelId = WeaklyTypedInt

-- | Model from col.models
data Model = Model {
    modelId        :: ModelId          -- ^ Id of a model.
  , modelCss       :: Text             -- ^ Style of cards.
  , modelDid       :: Value            -- TODO DeckId?
  , modelFlds      :: [ModelField]     -- ^ Description of fields.
  , modelLatexPre  :: Text             -- ^ TeX header
  , modelLatexPost :: Text             -- ^ Tex footer
  , modelName      :: String           -- ^ Name of a model.
  , modelSortf     :: Value            -- TODO Int?
  , modelTags      :: Value            -- TODO [wtf]?
  , modelTmpls     :: [ModelTemplate]  -- ^ Description of templates.
  , modelType      :: Value            -- TODO Bool?
  , modelVers      :: Value            -- TODO [wtf]?
  , modelMod       :: ModificationTime -- ^ Modification time.
  , modelUsn       :: Value            -- TODO Int?
  } deriving (Show, Eq, Generic)

instance Default Model where
  def = Model {
      modelId        = 0
    , modelCss       = ""
    , modelDid       = undefined
    , modelFlds      = []
    , modelLatexPre  = T.concat [
        "\\documentclass[12pt]{article}\n\\special{papersize=3in,5in}\n"
      , "\\usepackage[utf8]{inputenc}\n\\usepackage{amssymb,amsmath}\n"
      , "\\pagestyle{empty}\n\\setlength{\\parindent}{0in}\n"
      , "\\begin{document}\n"
      ]
    , modelLatexPost = "\\end{document}"
    , modelName      = ""
    , modelSortf     = undefined
    , modelTags      = undefined
    , modelTmpls     = []
    , modelType      = undefined
    , modelVers      = undefined
    , modelMod       = def
    , modelUsn       = undefined -- TODO
    }

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
