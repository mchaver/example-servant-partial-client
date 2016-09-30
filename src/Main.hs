{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as Map
import           Data.Text
import           Models.V3
import qualified Network.Wai.Handler.Warp as Warp
import           Server

main :: IO ()
main = do
  -- use an 'TVar (Map.Map Text UserV3)' as our database for testing.
  -- Text is a unique userIdent
  -- assume we have a server in its third version that supports old API queries
  -- there have been changes to our database but we store everything as UserV3
  -- and for old versions of the API we can convert to and from User to UserV3
  tVarUserDb <- newTVarIO (Map.fromList []) :: IO (TVar (Map.Map Text User))
  Warp.run 3000 $ app tVarUserDb
