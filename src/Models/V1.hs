{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.V1 where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User {
  userIdent :: Text -- | a unique username/handler/persona.
, name      :: Text -- | the user's name, not required to be unique.
, age       :: Int  -- | the user's age.
} deriving (Eq,Show,Generic)

instance ToJSON User
instance FromJSON User
