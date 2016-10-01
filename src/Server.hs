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
import qualified Models.V1 as V1
import qualified Models.V2 as V2
import qualified Models.V3 as V3
import           Network.Wai
import           Servant


server :: TVar (Map.Map Text V3.User) -> Server ServerApi
server tVarUserDb =
  (v1UserAddH :<|> v1UserGetH)
  :<|> (v2UserAddH :<|> v2UserGetH)
  :<|> (v3UserAddH :<|> v3UserUpdateH :<|> v3UserDeleteH :<|> v3UserExistsH :<|>  v3UserGetH)

  where
    v1UserAddH newUser   = liftIO $ v1UserAdd newUser
    v1UserGetH userIdent = liftIO $ v1UserGet userIdent

    v2UserAddH newUser   = liftIO $ v2UserAdd newUser
    v2UserGetH userIdent = liftIO $ v2UserGet userIdent

    v3UserAddH newUser                   = liftIO $ v3UserAdd newUser
    v3UserUpdateH userIdent existingUser = liftIO $ v3UserUpdate userIdent existingUser
    v3UserDeleteH userIdent              = liftIO $ v3UserDelete userIdent
    v3UserExistsH userIdent              = liftIO $ v3UserExists userIdent
    v3UserGetH userIdent                 = liftIO $ v3UserGet userIdent

    -- V1
    v1UserAdd :: V1.User -> IO Bool
    v1UserAdd newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup (V1.userIdent newUser) userDb of
        Nothing -> do
          _ <- atomically $ swapTVar tVarUserDb (Map.insert (V1.userIdent newUser) (V3.v1UserToV3User newUser) userDb)
          return True
        Just _  -> return False -- userIdent is not available

    v1UserGet :: Text -> IO (Maybe V1.User)
    v1UserGet userIdent = do
      userDb <- readTVarIO tVarUserDb
      return $ V3.v3UserToV1User <$> Map.lookup userIdent userDb

    -- V2
    v2UserAdd :: V2.User -> IO (Maybe V2.User)
    v2UserAdd newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup (V2.userIdent newUser) userDb of
        Nothing -> do
          _ <- atomically $ swapTVar tVarUserDb (Map.insert (V2.userIdent newUser) (V3.v2UserToV3User newUser) userDb)
          return $ Just newUser
        Just _  -> return Nothing

    v2UserGet :: Text -> IO (Maybe V2.User)
    v2UserGet userIdent = do
      userDb <- readTVarIO tVarUserDb
      return $ V3.v3UserToV2User <$> Map.lookup userIdent userDb


    -- V3
    v3UserAdd :: V3.User -> IO (Maybe V3.User)
    v3UserAdd newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup (V3.userIdent newUser) userDb of
        Nothing -> do
          _ <- atomically $ swapTVar tVarUserDb (Map.insert (V3.userIdent newUser) newUser userDb)
          return $ Just newUser
        Just _  -> return Nothing


    v3UserUpdate :: Text -> V3.User -> IO (Maybe V3.User)
    v3UserUpdate userIdent newUser = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup userIdent userDb of
        Nothing -> return Nothing
        Just _ -> do
          _ <- atomically $ swapTVar tVarUserDb (Map.insert (V3.userIdent newUser) newUser (Map.delete userIdent userDb))
          return $ Just newUser

    v3UserGet :: Text -> IO (Maybe V3.User)
    v3UserGet userIdent = do
      userDb <- readTVarIO tVarUserDb
      return $ Map.lookup userIdent userDb

    v3UserDelete :: Text -> IO Bool
    v3UserDelete userIdent = do
      userDb <- readTVarIO tVarUserDb
      _ <- atomically $ swapTVar tVarUserDb (Map.delete userIdent userDb)
      return True

    v3UserExists :: Text -> IO Bool
    v3UserExists userIdent = do
      userDb <- readTVarIO tVarUserDb
      case Map.lookup userIdent userDb of
        Nothing -> return False
        Just _  -> return True

serverApi :: Proxy ServerApi
serverApi = Proxy

app :: TVar (Map.Map Text V3.User) -> Application
app db = serve serverApi $ server db
