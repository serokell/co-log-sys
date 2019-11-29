module Colog.Syslog.Config
       ( -- * Configuration
         SyslogConfig (..)
         -- * Collector
       , Collector (..)
         -- * Re-exports
       , Family (..)
       , HostName
       , PortNumber
       ) where

import Colog.Syslog.Priority (Facility (..))

import Data.Aeson (FromJSON(..), ToJSON(..), Value (..), withText, withObject,
    withScientific, (.:), (.:?), (.!=), (.=), object)
import Data.Text (Text, pack, unpack)
import Fmt ((+|), (|+))
import Network.Socket (Family (..), HostName, PortNumber)
import Text.Read (readMaybe)

-- | Configuration for Syslog
data SyslogConfig = SyslogConfig
    { collector :: !Collector -- ^ Where the messages will be delivered
    , facility  :: !Facility  -- ^ Indicates the sending process type
    , appName   :: !Text      -- ^ A name to recognize the sending application
    } deriving (Show, Read, Eq)

instance FromJSON SyslogConfig where
    parseJSON = withObject "SyslogConfig" $ \v -> SyslogConfig
        <$> v .:? "collector" .!= AutoLocal
        <*> v .:? "facility"  .!= User
        <*> v .:  "app-name"

instance ToJSON SyslogConfig where
    toJSON SyslogConfig {..} = object
        [ "collector" .= collector
        , "facility"  .= facility
        , "app-name"  .= appName
        ]

-- | Represents a receiver for Syslog messages
data Collector
    = AutoLocal    -- ^ Automatic local syslog destination
    | Local String -- ^ Path to local Unix FIFO. Not supported under Windows.
    | Remote Family HostName PortNumber
    -- ^ Remote server, it's made of:
    -- Network Address 'Family' (usually AF_INET or AF_INET6)
    -- , remote 'HostName' (can also be localhost)
    -- and 'PortNumber' (usually 514 for syslog)
    deriving (Show, Read, Eq)

instance FromJSON Collector where
    parseJSON = withObject "Collector" $ \v -> do
        collectorType <- v .: "collector-type"
        case collectorType :: Text of
            "auto" -> return AutoLocal
            "local" -> Local
                <$> v .:? "fifo-path"   .!= "/dev/log"
            "remote" -> Remote
                <$> v .:? "family"      .!= AF_INET
                <*> v .:? "hostname"    .!= "localhost"
                <*> v .:? "port-number" .!= 514
            _ -> fail "Parsing Collector failed: unknown \"collector-type\""

instance ToJSON Collector where
    toJSON = \case
        AutoLocal -> object
            [ "collector-type" .= ("auto" :: Text)
            ]
        Local fifoPath -> object
            [ "collector-type" .= ("local" :: Text)
            , "fifo-path"        .= fifoPath
            ]
        Remote family hostname portNum -> object
            [ "collector-type" .= ("remote" :: Text)
            , "family"           .= family
            , "hostname"         .= hostname
            , "port-number"      .= portNum
            ]

-- Orphan Instances
instance FromJSON Family where
    parseJSON = withText "Family" $ \t ->
        maybe (fail $ "Unknown Family: \""+|t|+"\"") pure . readMaybe $ unpack t

instance ToJSON Family where
    toJSON = String . pack . show

instance FromJSON PortNumber where
    parseJSON = withScientific "PortNumber" $ pure . round

instance ToJSON PortNumber where
    toJSON = Number . fromIntegral
