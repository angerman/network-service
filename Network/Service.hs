{-# LANGUAGE Rank2Types #-}
module Network.Service where

import Data.ByteString (ByteString)

class ServiceMessage a where
  toBS :: a -> ByteString
  fromBS :: ByteString -> a

data Service a = Service { sDone :: IO Bool
                         , sRecv :: IO a
                         , sSend :: a -> IO ()
                         , sTerm :: IO ()
                         }

type ServiceHandler a = Service a -> IO ()
