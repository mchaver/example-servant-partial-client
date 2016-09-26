{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text
import Models
import Servant.API



-- | The 'ServerApi' implements all of the versions. Some routes may be the 
-- the same and some may change.
type ServerApi = ClientV1
            :<|> ClientV2
            :<|> ClientV3

-- | Add and get a 'User'.
type ClientV1 = "v1" :>
           (    "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] Bool -- add a User, returns a Bool to show if the data entry was succseful or not
           :<|> UserGetRoute
           )

-- | Add and get a 'User', but the add route returns 'Maybe User' instead of 'Bool'.
type ClientV2 = "v2" :>
          (    "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe User) -- add a User, returns a Maybe User to show if the data entry was succseful or not
          :<|> UserGetRoute
          )

-- | This route is shared by ClientV1 and ClientV2
type UserGetRoute = "user" :> "get" :> Capture "ident" Text  :> Get '[JSON] (Maybe User) -- get a User by name

-- | Add and get a 'UserV3'.
type ClientV3 = "v3" :> 
          (    "user" :> "add"    :> ReqBody '[JSON] UserV3 :> Post '[JSON] (Maybe UserV3) -- add a UserV3, returns a Maybe User to show if the data entry was succseful or not
          :<|> "user" :> "update" :> ReqBody '[JSON] UserV3  :> Post '[JSON] (Maybe UserV3) -- get a User by name
          :<|> "user" :> "delete" :> Capture "ident" Text  :> Post '[JSON] Bool -- delete a User by its ident
          :<|> "user" :> "exists" :> Capture "ident" Text  :> Get '[JSON] Bool -- see if a userIdent is available
          :<|> "user" :> "get"    :> Capture "ident" Text  :> Get '[JSON] (Maybe UserV3) -- get a User by name
          )

api :: Proxy ServerApi
api = Proxy