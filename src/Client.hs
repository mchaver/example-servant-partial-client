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
