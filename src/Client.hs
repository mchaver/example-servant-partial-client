module Client where

import           Api
import           Data.Text
import qualified Models.V1 as V1
import qualified Models.V3 as V3
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

-- V1

userAddV1 :: V1.User -> Manager -> BaseUrl -> ClientM Bool
userGetV1 :: Text -> Manager -> BaseUrl -> ClientM (Maybe V1.User)
userAddV1 :<|> userGetV1 = client clientApiV1

-- partial implementation of V3

userAddV3    :: V3.User -> Manager -> BaseUrl -> ClientM (Maybe V3.User)
userDeleteV3 :: Text -> Manager -> BaseUrl -> ClientM Bool
userGetV3    :: Text -> Manager -> BaseUrl -> ClientM (Maybe V3.User)
userAddV3 :<|> _ :<|> userDeleteV3 :<|> _ :<|> userGetV3 = client clientApiV3
