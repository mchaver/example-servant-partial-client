{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import           Api

import           Client

import           Control.Concurrent.STM.TVar
import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except

import qualified Data.Map.Strict as Map
import           Data.Text

import qualified Models.V1 as V1
import qualified Models.V3 as V3

import           Network.HTTP.Client
import           Network.Wai.Handler.Warp

import           Server

import           Servant.API
import           Servant.Client

import           Test.Hspec

spec :: Spec
spec = do
  around withApp $ do
    describe "V1" $ do
      it "/v1/user/get: returns Nothing for non-existing users" $ \port -> do
        try port (userGetV1 "foo") `shouldReturn` Nothing

      it "/v1/user/add: adds a user" $ \port -> do
        let user = V1.User "foo" "John Smith" 25
        try port (userAddV1 user) `shouldReturn` True

      it "/v1/user/get: finds a user that has been added to the database" $ \port -> do
        let user = V1.User "foo" "John Smith" 25
        try port (userAddV1 user) `shouldReturn` True
        try port (userGetV1 "foo") `shouldReturn` (Just user)

    describe "V3" $ do
      it "/v3/user/delete: delete an existing user" $ \port -> do
        let user = V3.User "foo" "John Smith" 25 "Washington, D.C." "888-888-8888"
        try port (userAddV3 user) `shouldReturn` (Just user)
        try port (userGetV3 "foo") `shouldReturn` (Just user)
        try port (userDeleteV3 "foo") `shouldReturn` True
        try port (userGetV3 "foo") `shouldReturn` Nothing

main :: IO ()
main = hspec spec

withApp :: (Int -> IO a) -> IO a
withApp action = do
  m <- newTVarIO (Map.fromList []) :: IO (TVar (Map.Map Text V3.User))
  testWithApplication (return $ app m) action

try :: Int -> (Manager -> BaseUrl -> ClientM a) -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runExceptT $ action manager baseUrl
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a -> return a
