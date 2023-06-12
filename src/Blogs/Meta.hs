{-# LANGUAGE DeriveGeneric #-}

module Blogs.Meta (BlogMeta (..)) where

import Data.Aeson
import GHC.Generics

data BlogMeta = BlogMeta
    { title_en :: String
    , title_cn :: String
    , ref_id :: String
    }
    deriving (Generic, Show)

instance FromJSON BlogMeta
