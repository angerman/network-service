{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Example.Echo
-- Copyright   : (c) Moritz Angermann 2014
-- License     : MIT
--
-- Maintainer  : moritz@lichtzwerge.de
-- Stability   : stable
-- Portability : portable
--
-- An echo service on port 2000
--
-- To start the server run
-- > $ echo server
--
-- You can then run the client
-- > $ echo client
--
-- Or connect to the server with telnet
-- > $ telnet localhost 2000
--
--------------------------------------------------------------------------------
module Main (main) where
import           Network.Simple.TCP
import           Data.ByteString.Char8   ( pack, unpack )
import           System.Environment      ( getProgName, getArgs )
import           Control.Monad           ( unless )

import Network.Service
import Network.Transport.Encoding.Base64 (mkService)

-- | Make a service server running on the executing host.
mkServiceServer :: ServiceMessage a
                   => ServiceName      -- ^ the port
                   -> ServiceHandler a -- ^ a handler to handle connections.
                   -> IO ()
mkServiceServer port shandler = serve HostAny port handler
  where handler :: (Socket, SockAddr) -> IO ()
        handler x = mkService x >>= shandler

-- | Make a service client
mkServiceClient :: ServiceMessage a
                   => HostName       -- ^ The hostname that provides a 'Service a'.
                   -> ServiceName    -- ^ The port the service runs at.
                   -> IO (Service a) -- ^ The client. (Service interface)
mkServiceClient h p = connect h p mkService

-- | We use a simple data type for communication.
--   All it holds is a simple string.
data Message = Msg String

-- | Turning a String message into a bytestring.
--   Trivially through pack and unpack.
instance ServiceMessage Message where
  toBS (Msg s) = pack s
  fromBS = Msg . unpack

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  
  case args of
    ["server"] -> mkServiceServer "2000" handler
    ["client"] -> do
      service <- mkServiceClient "localhost" "2000"
      let msg = "Hello World!"
      putStrLn $ "[Client] Sending: " ++ msg
      sSend service $ Msg msg
      Msg resp <- sRecv service
      putStrLn $ "[Client] Received: " ++ resp
      sTerm service
    _          -> putStrLn $ "Usage: " ++ prog ++ " (server|client)"

  where handler :: ServiceHandler Message
        handler service = sRecv service >>= sSend service >> sDone service >>= flip unless (handler service)
