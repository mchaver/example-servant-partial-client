{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.V3 where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import qualified Models.V1 as V1

-- | This User model is adds two fields to the User model from V1 and V2.
data User = User {
  userIdent   :: Text -- | a unique username/handler/persona
, name        :: Text -- | the user's name, not required to be unique
, age         :: Int  -- | the user's age.
, address     :: Text -- | the user's address.
, phoneNumber :: Text -- | the user's phone number.
} deriving (Eq,Show,Generic)

instance ToJSON User
instance FromJSON User

-- | Convert V1.User to V3.User
v1UserToV3User :: V1.User -> User
v1UserToV3User u = User (V1.userIdent u) (V1.name u) (V1.age u) "" ""

-- | Convert V3.User to V1.User
v3UserToV1User :: User -> V1.User
v3UserToV1User u = V1.User (userIdent u) (name u) (age u)
