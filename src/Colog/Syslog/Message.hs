module Colog.Syslog.Message
       ( -- * Message type
         Message (..)
         -- * Lenses
       , severity
       , content
         -- * Formatting
       , fmtMessageFlat
       , fmtMessageColored
       ) where

import Colog.Syslog.Priority (Severity(..))

import Fmt ((|+), (|++|))
import Lens.Micro (Lens', lens)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
    ConsoleLayer (Foreground), SGR (SetColor, Reset), setSGRCode)

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

-- | Formatting function to 'Text' (without colored 'Severity')
fmtMessageFlat :: Message -> Text
fmtMessageFlat Message {..} = msgSeverity|++|msgContent|+""

-- | Formatting function to 'Text' with colored 'Severity'
fmtMessageColored :: Message -> Text
fmtMessageColored Message {..} = withColor|++|msgSeverity|++|resetColor|++|msgContent|+""
  where
    color = case msgSeverity of
        Emergency -> Red
        Alert     -> Red
        Critical  -> Red
        Error     -> Red
        Warning   -> Yellow
        Notice    -> Blue
        Info      -> White
        Debug     -> Green
    withColor = toText (setSGRCode [SetColor Foreground Vivid color])
    resetColor = toText (setSGRCode [Reset])
