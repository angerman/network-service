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

mkService :: ServiceMessage a => (Socket, SockAddr) -> IO (Service a)
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
