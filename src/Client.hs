module Client where

import           Api
import           Data.Text
import           Models
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

-- V1

userAddV1 :: User -> Manager -> BaseUrl -> ClientM Bool
userGetV1 :: Text -> Manager -> BaseUrl -> ClientM (Maybe User)
userAddV1 :<|> userGetV1 = client clientApiV1

-- partial implementation of V3

userAddV3    :: UserV3 -> Manager -> BaseUrl -> ClientM (Maybe UserV3)
userDeleteV3 :: Text -> Manager -> BaseUrl -> ClientM Bool
userGetV3    :: Text -> Manager -> BaseUrl -> ClientM (Maybe UserV3)
userAddV3 :<|> _ :<|> userDeleteV3 :<|> _ :<|> userGetV3 = client clientApiV3
