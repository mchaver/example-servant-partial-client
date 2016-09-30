{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text
import qualified Models.V1 as V1
import qualified Models.V3 as V3
import           Servant.API

-- | The 'ServerApi' implements all of the versions. Some routes may be the
-- the same and some may change.
type ServerApi = ClientApiV1
            :<|> ClientApiV2
            :<|> ClientApiV3

-- | Add and get a 'User'.
type ClientApiV1 = "v1" :>
           (    "user" :> "add" :> ReqBody '[JSON] V1.User :> Post '[JSON] Bool -- add a User, returns a Bool to show if the data entry was succseful or not
           :<|> UserGetRoute
           )

-- | Add and get a 'User', but the add route returns 'Maybe User' instead of 'Bool'.
type ClientApiV2 = "v2" :>
          (    "user" :> "add" :> ReqBody '[JSON] V1.User :> Post '[JSON] (Maybe V1.User) -- add a User, returns a Maybe User to show if the data entry was succseful or not
          :<|> UserGetRoute
          )

-- | This route is shared by ClientApiV1 and ClientApiV2.
type UserGetRoute = "user" :> "get" :> Capture "ident" Text  :> Get '[JSON] (Maybe V1.User) -- get a User by name

-- | Add and get a 'UserV3'.
type ClientApiV3 = "v3" :>
          (    "user" :> "add"    :> ReqBody '[JSON] V3.User :> Post '[JSON] (Maybe V3.User) -- add a UserV3, returns a Maybe User to show if the data entry was succseful or not
          :<|> "user" :> "update" :> Capture "ident" Text  :> ReqBody '[JSON] V3.User  :> Post '[JSON] (Maybe V3.User) -- get a User by name
          :<|> "user" :> "delete" :> Capture "ident" Text  :> Post '[JSON] Bool -- delete a User by its ident
          :<|> "user" :> "exists" :> Capture "ident" Text  :> Get '[JSON] Bool -- see if a userIdent is available
          :<|> "user" :> "get"    :> Capture "ident" Text  :> Get '[JSON] (Maybe V3.User) -- get a User by name
          )

api :: Proxy ServerApi
api = Proxy

clientApiV1 :: Proxy ClientApiV1
clientApiV1 = Proxy

clientApiV2 :: Proxy ClientApiV2
clientApiV2 = Proxy

clientApiV3 :: Proxy ClientApiV3
clientApiV3 = Proxy
