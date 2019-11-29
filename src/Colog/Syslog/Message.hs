module Colog.Syslog.Message
       ( -- * Message type
         Message (..)
         -- * Lenses
       , severity
       , content
         -- * Formatting
       , fmtMessageFlat
       ) where

import Colog.Syslog.Priority (Severity(..))

import Data.Text (Text)
import Fmt ((|+), (|++|))
import Lens.Micro (Lens', lens)

-- | This Type contains the info strictly REQUIRED by a syslog message
data Message = Message
    { msgSeverity :: !Severity -- ^ message 'Severity'
    , msgContent  :: !Text     -- ^ message content
    } deriving (Show)

-- | 'Lens' for a 'Message' severity
severity :: Lens' Message Severity
severity = lens msgSeverity $ \message svr -> message { msgSeverity = svr }

-- | 'Lens' for a 'Message' content
content :: Lens' Message Text
content = lens msgContent $ \message cnt -> message { msgContent = cnt }

-- | Simple formatting function to 'Text'
fmtMessageFlat :: Message -> Text
fmtMessageFlat Message {..} = msgSeverity|++|msgContent|+""
