module Colog.Syslog.Priority
       ( -- * Priority
         Priority (..)
       , priorityValue
         -- * Severity
       , Severity (..)
       , severityCode
         -- * Facility
       , Facility (..)
       , facilityCode
       ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value (..), withText, withObject,
    (.:), (.=), object)
import Data.Bits ((.|.), shiftL)
import Fmt (Buildable (build), fmt, (+|), (|+))
import Text.Show (Show (show))

-- | Represents the priority of a syslog message, as per RFC5424
data Priority = Priority Facility Severity
    deriving (Eq, Read)

-- NOTE: the angular parenthesis are important: they are required by the protocol
instance Buildable Priority where
    build p = "<"+|(priorityValue p)|+">"

instance Show Priority where
    show = fmt . build

instance FromJSON Priority where
    parseJSON = withObject "Priority" $ \v -> Priority
        <$> v .: "facility"
        <*> v .: "severity"

instance ToJSON Priority where
    toJSON (Priority facility severity) = object
        [ "facility" .= facility
        , "severity" .= severity
        ]

-- | Calculate the 'Priority' value of a message, as per RFC5424
priorityValue :: Priority -> Int
priorityValue (Priority fac sev) = facilityCode fac .|. severityCode sev


-- | Represents the severity of a syslog message, as per RFC5424
data Severity
    = Emergency  -- ^ System is unusable
    | Alert      -- ^ Action must be taken immediately
    | Critical   -- ^ Critical conditions
    | Error      -- ^ Error conditions
    | Warning    -- ^ Warning conditions
    | Notice     -- ^ Normal but significant condition
    | Info       -- ^ Informational messages
    | Debug      -- ^ Debug-level messages
    deriving (Eq, Show, Read, Bounded, Enum)

-- | 'Ord' instance uses the 'Enum' instance reversed, so Severities can be
-- ordered correctly and 'Enum' can give us the right 'severityCode'
instance Ord Severity where
    compare l r = compare (fromEnum r) (fromEnum l)

instance Buildable Severity where
    build = \case
        Emergency -> "[Emergency] "
        Alert     -> "[Alert]     "
        Critical  -> "[Critical]  "
        Error     -> "[Error]     "
        Warning   -> "[Warning]   "
        Notice    -> "[Notice]    "
        Info      -> "[Info]      "
        Debug     -> "[Debug]     "

instance FromJSON Severity where
    parseJSON = withText "Severity" $ \t ->
        maybe (fail $ "Unknown Severity: \""+|t|+"\"") pure . readMaybe $ toString t

instance ToJSON Severity where
    toJSON = String . Prelude.show

-- | Numerical code for a 'Severity'. Used to calculate the 'priorityValue'.
severityCode :: Severity -> Int
severityCode = fromEnum


-- | Represents the machine process that created a syslog event, as per RFC5424
data Facility
    = Kernel    -- ^ Kernel messages
    | User      -- ^ User-level messages. Unless something else applies, this should be your first choice.
    | Mail      -- ^ Mail system
    | Daemon    -- ^ System daemons
    | Auth      -- ^ Security/Authorization messages
    | Syslog    -- ^ Messages generated internally by syslogd
    | Lpr       -- ^ Line printer subsystem
    | News      -- ^ Network news subsystem
    | Uucp      -- ^ UUCP subsystem
    | Cron      -- ^ Cron messages
    | AuthPriv  -- ^ Private Security/Authorization messages
    | Ftp       -- ^ FTP daemon
    | Ntp       -- ^ NTP subsystem
    | LogAudit  -- ^ Log audit
    | LogAlert  -- ^ Log alert
    | Clock     -- ^ Clock daemon
    | Local0    -- ^ LOCAL 0 to 7 are not used by UNIX and can be used freely
    | Local1
    | Local2
    | Local3
    | Local4
    | Local5
    | Local6
    | Local7
    deriving (Eq, Show, Read, Bounded, Enum)

instance FromJSON Facility where
    parseJSON = withText "Facility" $ \t ->
        maybe (fail $ "Unknown Facility: \""+|t|+"\"") pure . readMaybe $ toString t

instance ToJSON Facility where
    toJSON = String . Prelude.show

-- | Numerical code for a 'Facility'. Used to calculate the 'priorityValue'.
facilityCode :: Facility -> Int
facilityCode fac = shiftL (fromEnum fac) 3
