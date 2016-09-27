{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.V1 where

import Data.Aeson
import Data.Text
import GHC.Generics

-- | This User model is for V1 and V2. Models V2 will directly import and
-- re-export this.
data User = User {
  userIdent :: Text -- | a unique username/handler/persona.
, name      :: Text -- | the user's name, not required to be unique.
, age       :: Int  -- | the user's age.
} deriving (Eq,Show,Generic)

instance ToJSON User
instance FromJSON User
