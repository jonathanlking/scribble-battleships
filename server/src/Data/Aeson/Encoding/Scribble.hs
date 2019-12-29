{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Scribble PureScript compatible JSON encoding.
-- | Designed to be used with the 'DerivingVia' extension:
--
-- @
-- data Message Text
--   deriving (Generic, Show)
--   deriving (ToJSON, FromJSON) via (ScribbleJSON Message)
-- @

module Data.Aeson.Encoding.Scribble
  ( ScribbleJSON (..)
  ) where

import Prelude
import Data.Aeson
import Data.Aeson.Types (Options, defaultOptions, Parser)
import Data.Coerce (coerce)
import GHC.Generics (Generic, Rep)

newtype ScribbleJSON a = ScribbleJSON a

scribbleOptions :: Options
scribbleOptions
  = defaultOptions
    { tagSingleConstructors = True,
      sumEncoding = TaggedObject "tag" "values",
      omitNothingFields = False,
      unwrapUnaryRecords = False
    }

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (ScribbleJSON a) where
  toJSON = genericToJSON scribbleOptions . coerce @_ @a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (ScribbleJSON a) where
  parseJSON = coerce @(Parser a) . genericParseJSON scribbleOptions
