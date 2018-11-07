module Colog.Syslog.Handler
       ( SyslogHandler
       , mkSyslogHandler
       , withSyslog
       , logSyslogMessage
       ) where

import Colog.Core.Action (LogAction (..))

import Colog.Syslog.Config
import Colog.Syslog.Message
import Colog.Syslog.Priority

import System.Info (os)

import Control.Exception (IOException)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Posix.Process (getProcessID)
import Fmt ((+|), (|+), (|++|), (+||), (||+))

-- | An Handler for Syslog connections
data SyslogHandler = SyslogHandler
    { shSend  :: Message -> IO ()
    , shClose :: IO ()
    }

-- | Creates a 'SyslogHandler' from a 'SyslogConfig'
mkSyslogHandler :: SyslogConfig -> IO SyslogHandler
mkSyslogHandler SyslogConfig {..} = do
    (skt, sAddr, sType) <- openSyslogSocket collector
    let shSend = \Message {..} -> do
            pid <- if
                | onWindows -> return "[windows]"
                | otherwise -> getProcessID >>= \p -> return ("["+||p||+"]" :: Text)
            let priority = Priority facility msgSeverity
                msg = encodeUtf8
                    (""+|priority|++|appName|++|pid|+": "+|msgContent|+"\0" :: Text)
            void $ case sType of
                Datagram -> sendTo skt msg sAddr
                _        -> send skt msg -- 'Stream' is the only possibility here
        shClose = close skt
    return $ SyslogHandler {..}

-- | Opens a connection to the specified 'Collector', gives back socket's infos
openSyslogSocket :: Collector -> IO (Socket, SockAddr, SocketType)
openSyslogSocket = \case
    AutoLocal -> if
        | onWindows -> openSyslogSocket $ Remote AF_INET "localhost" 514
        | onMacOs   -> openSyslogSocket $ Local "/var/run/syslog"
        | otherwise -> openSyslogSocket $ Local "/dev/log"
    Local path -> if 
        | onWindows -> fail "Local is not supported on Windows, you'll \
                            \probably want to use AutoLocal instead"
        | otherwise -> do
            let unixAddr = SockAddrUnix path
            -- make a 'Stream' socket and try to connect to it
            skt <- socket AF_UNIX Stream defaultProtocol
            tryRes <- try $ connect skt unixAddr
            -- if it failed, use a 'Datagram' socket
            case tryRes :: Either IOException () of
                Right _ -> return (skt, unixAddr, Stream)
                Left _  -> do
                    close skt
                    dskt <- socket AF_UNIX Datagram defaultProtocol
                    return (dskt, unixAddr, Datagram)
    Remote family hostName port -> do
        let hints = defaultHints
                { addrFlags      = [AI_NUMERICSERV]
                , addrFamily     = family
                , addrSocketType = Datagram
                }
        addrInfo:_ <- getAddrInfo (Just hints) (Just hostName) (Just $ show port)
        skt <- socket family Datagram defaultProtocol
        return (skt, addrAddress addrInfo, Datagram)

-- | Uses continuation-passing style for Syslog, similar to 'withFile' with 'Handle's
withSyslog :: SyslogConfig -> (SyslogHandler -> IO r) -> IO r
withSyslog config = bracket (mkSyslogHandler config) shClose

-- | Uses a 'SyslogHandler' to make a 'LogAction' that logs 'Message's
logSyslogMessage :: MonadIO m => SyslogHandler -> LogAction m Message
logSyslogMessage SyslogHandler{..} = LogAction $ liftIO . shSend

-- Utility definitions for OS checking
onWindows :: Bool
onWindows = os == "mingw32"

onMacOs :: Bool
onMacOs = os == "darwin"
