{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import Data.Aeson
import Data.Text
import GHC.Generics

-- | This User model is for V1 and V2.
data User = User {
  userIdent :: Text -- | a unique username/handler/persona
, name      :: Text -- | the user's name, not required to be unique
, age       :: Int
} deriving (Eq,Show,Generic)

instance ToJSON User
instance FromJSON User

-- | This User model is for V3. It adds two fields.
data UserV3 = UserV3 {
  uv3UserIdent   :: Text -- | a unique username/handler/persona
, uv3Name        :: Text -- | the user's name, not required to be unique
, uv3Age         :: Int
, uv3Address     :: Text
, uv3PhoneNumber :: Text
} deriving (Eq,Show,Generic)

instance ToJSON UserV3
instance FromJSON UserV3

-- | Convert User to UserV3
userToUserV3 :: User -> UserV3
userToUserV3 u = UserV3 (userIdent u) (name u) (age u) "" ""

-- | Convert UserV3 to User
userV3ToUser :: UserV3 -> User
userV3ToUser u = User (uv3UserIdent u) (uv3Name u) (uv3Age u)
