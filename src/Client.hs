module Client where

import           Api
import           Data.Text
import qualified Models.V1 as V1
import qualified Models.V2 as V2
import qualified Models.V3 as V3
import           Network.HTTP.Client
import           Servant
import           Servant.Client

-- | Full client implementation of V1.

userAddV1 :: V1.User -> Manager -> BaseUrl -> ClientM Bool
userGetV1 :: Text -> Manager -> BaseUrl -> ClientM (Maybe V1.User)
userAddV1 :<|> userGetV1 = client clientApiV1

-- | Partial client implementation of V2. Use '_' to leave routes undeclared.
-- Here we are not declaring userGetV2.

userAddV2 :: V2.User -> Manager -> BaseUrl -> ClientM (Maybe V2.User)
userAddV2 :<|> _ = client clientApiV2

-- | Partial client implementation of V3. Use '_' to leave routes undeclared.
-- Here we are not declaring userExistsV3.

userAddV3    :: V3.User -> Manager -> BaseUrl -> ClientM (Maybe V3.User)
userUpdateV3 :: Text -> V3.User -> Manager -> BaseUrl -> ClientM (Maybe V3.User)
userDeleteV3 :: Text -> Manager -> BaseUrl -> ClientM Bool
userGetV3    :: Text -> Manager -> BaseUrl -> ClientM (Maybe V3.User)
userAddV3 :<|> userUpdateV3 :<|> userDeleteV3 :<|> _ :<|> userGetV3 = client clientApiV3
