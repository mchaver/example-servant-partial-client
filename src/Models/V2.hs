{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.V2 where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import qualified Models.V1 as V1


-- | The models in V2 are the same as V1, however the routes are slightly
-- different. You could use the same models from V1 but it would make the
-- semantics of the code less clear.

data User = User {
  userIdent :: Text -- | a unique username/handler/persona.
, name      :: Text -- | the user's name, not required to be unique.
, age       :: Int  -- | the user's age.
} deriving (Eq,Show,Generic)

instance ToJSON User
instance FromJSON User

-- | Convert V1.User to V2.User
v1UserToV2User :: V1.User -> User
v1UserToV2User u = User (V1.userIdent u) (V1.name u) (V1.age u)

-- | Convert V2.User to V1.User
v2UserToV1User :: User -> V1.User
v2UserToV1User u = V1.User (userIdent u) (name u) (age u)
