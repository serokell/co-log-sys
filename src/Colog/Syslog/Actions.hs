module Colog.Syslog.Actions
       ( -- * Syslog actions
         withLogMessageSyslog
         -- * 'Message' actions
       , logMessageStdout
       , logMessageStderr
       , logMessageHandle
       , withLogMessageFile
       ) where

import Colog.Actions (logTextStdout, logTextStderr, logTextHandle, withLogTextFile)
import Colog.Core.Action (LogAction (..), cmap)

import Colog.Syslog.Config (SyslogConfig)
import Colog.Syslog.Handler (withSyslog, logSyslogMessage)
import Colog.Syslog.Message (Message, fmtMessageColored, fmtMessageFlat)

-- | Sends a log using Syslog, implemented using continuation-passing style
-- (like for printing on file) because it's more efficient to open the connection
-- once at the beginning and it guarantees that the socket used will get closed.
withLogMessageSyslog :: MonadIO m => SyslogConfig -> (LogAction m Message -> IO r) -> IO r
withLogMessageSyslog config action = withSyslog config $ action . logSyslogMessage

-- | 'LogAction' to print a colored 'Message' to stdout
logMessageStdout :: MonadIO m => LogAction m Message
logMessageStdout = cmap fmtMessageColored logTextStdout

-- | 'LogAction' to print a colored 'Message' to stderr
logMessageStderr :: MonadIO m => LogAction m Message
logMessageStderr = cmap fmtMessageColored logTextStderr

-- | 'LogAction' to print a colored 'Message' to an 'Handle'
logMessageHandle :: MonadIO m => Handle -> LogAction m Message
logMessageHandle handle = cmap fmtMessageColored $ logTextHandle handle

-- | 'LogAction' to print a colored 'Message' to a file, using continuation-passing style
withLogMessageFile :: MonadIO m => FilePath -> (LogAction m Message -> IO r) -> IO r
withLogMessageFile path action = withLogTextFile path $ action . cmap fmtMessageFlat
