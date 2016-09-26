{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Api
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import           Data.Text
import           Models
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant


server :: MVar (Map.Map Text UserV3) -> Server ServerApi
server mVarUserDb = 
  (v1UserAddH :<|> v1UserGetH)
  :<|> (v2UserAddH :<|> v2UserGetH)
  :<|> (v3UserAddH :<|> v3UserUpdateH :<|> v3UserDeleteH :<|> v3UserExistsH :<|>  v3UserGetH)
    
  where
    v1UserAddH newUser = liftIO $ v1UserAdd newUser
    v1UserGetH name    = liftIO $ v1UserGet name

    v2UserAddH newUser = liftIO $ v2UserAdd newUser
    v2UserGetH name    = liftIO $ v1UserGet name

    v3UserAddH newUser = liftIO $ v3UserAdd newUser
    v3UserUpdateH existingUser = liftIO $ v3UserUpdate existingUser
    v3UserDeleteH name = liftIO $ v3UserDelete name
    v3UserExistsH name = liftIO $ v3UserExists name
    v3UserGetH name    = liftIO $ v3UserGet name
    
    -- V1
    v1UserAdd :: User -> IO Bool
    v1UserAdd newUser = do
      userDb <- takeMVar mVarUserDb
      case Map.lookup (userIdent newUser) userDb of
        Nothing -> do
          putMVar mVarUserDb (Map.insert (userIdent newUser) (userToUserV3 newUser) userDb)
          return True
        Just _  -> return False -- userIdent is not available
        
    v1UserGet :: Text -> IO (Maybe User)
    v1UserGet name = do
      userDb <- takeMVar mVarUserDb
      return $ userV3ToUser <$> Map.lookup name userDb

    -- V2
    v2UserAdd :: User -> IO (Maybe User)
    v2UserAdd newUser = do
      userDb <- takeMVar mVarUserDb
      case Map.lookup (userIdent newUser) userDb of
        Nothing -> do
          putMVar mVarUserDb (Map.insert (userIdent newUser) (userToUserV3 newUser) userDb)
          return $ Just newUser
        Just _  -> return Nothing
      
    
    -- V3
    v3UserAdd :: UserV3 -> IO (Maybe UserV3)
    v3UserAdd newUser = undefined

    v3UserUpdate :: UserV3 -> IO (Maybe UserV3)
    v3UserUpdate newUser = undefined
    
    v3UserGet :: Text -> IO (Maybe UserV3)
    v3UserGet name = undefined    

    v3UserDelete :: Text -> IO Bool
    v3UserDelete name = undefined    

    v3UserExists :: Text -> IO Bool
    v3UserExists name = undefined    


userToUserV3 :: User -> UserV3
userToUserV3 u = UserV3 (userIdent u) (name u) (age u) "" ""

userV3ToUser :: UserV3 -> User
userV3ToUser u = User (uv3UserIdent u) (uv3Name u) (uv3Age u)


serverApi :: Proxy ServerApi
serverApi = Proxy

app :: MVar (Map.Map Text UserV3) -> Application
app db = serve serverApi $ server db

main :: IO ()
main = do
  -- use an 'MVar (Map.Map Text UserV3)' as our database for testing.
  -- Text is a unique userIdent
  -- assume we have a server in its third version that supports old API queries
  -- there have been changes to our database but we store everything as UserV3
  -- and for old versions of the API we can convert to and from User to UserV3
  m <- newMVar (Map.fromList []) :: IO (MVar (Map.Map Text UserV3))
  Warp.run 3000 $ app m
