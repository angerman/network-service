{-# LANGUAGE Rank2Types #-}
module Main (main) where
import           Network.Simple.TCP
import           Data.ByteString.Char8   ( pack, unpack )
import           System.Environment      ( getProgName, getArgs )
import           Control.Monad           ( unless )

import Network.Service
import Network.Transport.Encoding.Base64 (mkService)

mkServiceServer :: ServiceMessage a => ServiceName -> ServiceHandler a -> IO ()
mkServiceServer port shandler = serve HostAny port handler
  where handler :: (Socket, SockAddr) -> IO ()
        handler x = mkService x >>= shandler

mkServiceClient :: ServiceMessage a => HostName -> ServiceName -> IO (Service a)
mkServiceClient h p = connect h p mkService

data Message = Msg String

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
