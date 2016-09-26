{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Api
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Map.Strict as Map
import           Data.Text
import           Models
import           Network.Wai
import           Servant


server :: TVar (Map.Map Text UserV3) -> Server ServerApi
server tVarUserDb =
  (v1UserAddH :<|> v1UserGetH)
  :<|> (v2UserAddH :<|> v2UserGetH)
  :<|> (v3UserAddH :<|> v3UserUpdateH :<|> v3UserDeleteH :<|> v3UserExistsH :<|>  v3UserGetH)

  where
    v1UserAddH newUser = liftIO $ v1UserAdd newUser
    v1UserGetH name    = liftIO $ v1UserGet name

    v2UserAddH newUser = liftIO $ v2UserAdd newUser
    v2UserGetH name    = liftIO $ v1UserGet name

    v3UserAddH newUser         = liftIO $ v3UserAdd newUser
    v3UserUpdateH existingUser = liftIO $ v3UserUpdate existingUser
    v3UserDeleteH name         = liftIO $ v3UserDelete name
    v3UserExistsH name         = liftIO $ v3UserExists name
    v3UserGetH name            = liftIO $ v3UserGet name

    -- V1
    v1UserAdd :: User -> IO Bool
    v1UserAdd newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup (userIdent newUser) userDb of
        Nothing -> do
          atomically $ swapTVar tVarUserDb (Map.insert (userIdent newUser) (userToUserV3 newUser) userDb)
          return True
        Just _  -> return False -- userIdent is not available

    v1UserGet :: Text -> IO (Maybe User)
    v1UserGet name = do
      userDb <- readTVarIO tVarUserDb
      return $ userV3ToUser <$> Map.lookup name userDb

    -- V2
    v2UserAdd :: User -> IO (Maybe User)
    v2UserAdd newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup (userIdent newUser) userDb of
        Nothing -> do
          atomically $ swapTVar tVarUserDb (Map.insert (userIdent newUser) (userToUserV3 newUser) userDb)
          return $ Just newUser
        Just _  -> return Nothing


    -- V3
    v3UserAdd :: UserV3 -> IO (Maybe UserV3)
    v3UserAdd newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup (uv3UserIdent newUser) userDb of
        Nothing -> do
          atomically $ swapTVar tVarUserDb (Map.insert (uv3UserIdent newUser) newUser userDb)
          return $ Just newUser
        Just _  -> return Nothing


    v3UserUpdate :: UserV3 -> IO (Maybe UserV3)
    v3UserUpdate newUser = undefined

    v3UserGet :: Text -> IO (Maybe UserV3)
    v3UserGet name = do
      userDb <- readTVarIO tVarUserDb
      return $ Map.lookup name userDb

    v3UserDelete :: Text -> IO Bool
    v3UserDelete name = do
      userDb <- readTVarIO tVarUserDb
      atomically $ swapTVar tVarUserDb (Map.delete name userDb)
      return True


    v3UserExists :: Text -> IO Bool
    v3UserExists name = undefined

serverApi :: Proxy ServerApi
serverApi = Proxy

app :: TVar (Map.Map Text UserV3) -> Application
app db = serve serverApi $ server db
