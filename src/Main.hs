{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Api
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import           Data.Text
import           Models
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Server

main :: IO ()
main = do
  -- use an 'MVar (Map.Map Text UserV3)' as our database for testing.
  -- Text is a unique userIdent
  -- assume we have a server in its third version that supports old API queries
  -- there have been changes to our database but we store everything as UserV3
  -- and for old versions of the API we can convert to and from User to UserV3
  m <- newTVarIO (Map.fromList []) :: IO (TVar (Map.Map Text UserV3))
  Warp.run 3000 $ app m
