{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Service
-- Copyright   : (c) Moritz Angermann 2014
-- License     : MIT
--
-- Maintainer  : moritz@lichtzwerge.de
-- Stability   : stable
-- Portability : portable
--
-- A service is an endpoint that can receive and send messages.
--
--------------------------------------------------------------------------------
module Network.Service where

import Data.ByteString (ByteString)

-- | Services operate on Messages, the ServiceMessage class abstracts
--   the serialization.
class ServiceMessage a where
  toBS :: a -> ByteString   -- ^ serialization to ByteString representation
  fromBS :: ByteString -> a -- ^ deserialization from ByteString representation

-- | A Service of a certain Message data.
data Service a = Service
                 { sDone :: IO Bool    -- ^ tells whether the service is done or not.
                 , sRecv :: IO a       -- ^ receive a message.
                 , sSend :: a -> IO () -- ^ send a message.
                 , sTerm :: IO ()      -- ^ terminate the service.
                 }
-- | A handler takes a service and returns an IO action.
--   The idea is that a service handler will 'run' a service
--
-- a simple echo handler might look like the following:
--
-- > handler service = do { msg <- sRecv service
-- >                      ; sSend service msg
-- >                      ; isDone <- sDone service
-- >                      ; unless isDone (handler service)
-- >                      }
type ServiceHandler a = Service a -> IO ()
