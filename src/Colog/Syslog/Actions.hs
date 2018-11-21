module Colog.Syslog.Actions
       ( -- * Syslog actions
         withLogMessageSyslog
       , withLogMessageSyslogGeneric
       ) where

import Universum

import Colog.Core.Action (LogAction (..))

import Colog.Syslog.Config
import Colog.Syslog.Handler 
import Colog.Syslog.Message

import Control.Monad.Trans.Control (MonadBaseControl)

-- | Sends a log using Syslog, implemented using continuation-passing style
-- (like for printing on file) because it's more efficient to open the connection
-- once at the beginning and it guarantees that the socket used will get closed.
withLogMessageSyslog :: MonadIO m => SyslogConfig -> (LogAction m Message -> IO r) -> IO r
withLogMessageSyslog config action = withSyslog config $ action . logSyslogMessage

-- | Like 'withLogMessageSyslog', but without the IO restriction on the continuation
-- function. NOTE: this allows more flexibility, but may also be slower
withLogMessageSyslogGeneric
    :: (MonadBaseControl IO n, MonadIO m)
    => SyslogConfig
    -> (LogAction m Message -> n r) -> n r
withLogMessageSyslogGeneric config action = withSyslogGeneric config $
    action . logSyslogMessage
