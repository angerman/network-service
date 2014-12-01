--------------------------------------------------------------------------------
-- |
-- Module      : Network.Transport.Encoding.Base64
-- Copyright   : (c) Moritz Angermann 2014
-- License     : MIT
--
-- Maintainer  : moritz@lichtzwerge.de
-- Stability   : stable
-- Portability : portable
--
-- A trivial service that uses base64 as encoding for the
-- messages and newlines for message separation.
--------------------------------------------------------------------------------
module Network.Transport.Encoding.Base64 (mkService) where
import           Network.Socket          (Socket, SockAddr, socketToHandle)
import           System.IO               (hSetBuffering, hGetContents, hPutStrLn, hClose
                                         ,IOMode( ReadWriteMode ), BufferMode( LineBuffering ))
import           Control.Concurrent      (newMVar, modifyMVar)
import           Data.ByteString.Base64  (encode, decode)
import           Data.ByteString.Char8   (pack, unpack)
import           Data.ByteString         (ByteString)

import           Network.Service

-- hack.
decode' :: ByteString -> ByteString
decode' bs = let Right res = decode bs in res

-- | Builds a simple service, that uses base64 as the base
--   encoding for the messages.  Messages are separated by
--   newlines.
mkService :: ServiceMessage a
             => (Socket, SockAddr) -- ^ The socket and socket address to the service is bound on.
             -> IO (Service a)     -- ^ The service to be used.
mkService (sock, addr) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl LineBuffering
  messages <- lines `fmap` (hGetContents hdl)
  messageMVar <- newMVar messages
  return $ Service { sDone = modifyMVar messageMVar
                             (\ms -> return (ms, ms == []))
                   , sRecv = modifyMVar messageMVar
                             (\ms -> return (tail ms, fromBS . decode' . pack $ head ms))
                   , sSend = hPutStrLn hdl . unpack . encode . toBS
                   , sTerm = hClose hdl
                   }
