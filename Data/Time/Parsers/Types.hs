{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Time.Parsers.Types where

import Control.Monad.Reader        (ReaderT)
import Data.Attoparsec.Char8       (Parser)
import Data.Attoparsec.FastSet     (FastSet)
import Data.Convertible
import Data.Convertible.Instances()
import qualified Data.Set          as Set
import Data.Time
import Data.Time.Clock.POSIX

-- | A type that can be converted from ZonedTime
class FromZonedTime a where
    fromZonedTime :: ZonedTime -> a

instance FromZonedTime ZonedTime where
    fromZonedTime = id

instance FromZonedTime LocalTime where
    fromZonedTime = zonedTimeToLocalTime

instance FromZonedTime Day where
    fromZonedTime = localDay . fromZonedTime

instance FromZonedTime UTCTime where
    fromZonedTime = zonedTimeToUTC

instance FromZonedTime POSIXTime where
    fromZonedTime = convert

-- | A type that can be converted to ZonedTime
-- For LocalTime, it is assumed the TimeZone is UTC
-- For Day, it is assumed that the TimeOfDay is midnight and the TimeZone is UTC
class ToZonedTime a where
    toZonedTime :: a -> ZonedTime

instance ToZonedTime ZonedTime where
    toZonedTime = id

instance ToZonedTime LocalTime where
    toZonedTime = flip ZonedTime utc

instance ToZonedTime Day where
    toZonedTime = toZonedTime . flip LocalTime midnight

instance ToZonedTime UTCTime where
    toZonedTime = utcToZonedTime utc

instance ToZonedTime POSIXTime where
    toZonedTime = convert

-- | Formats for purely numeric dates, e.g. 99-2-27
data DateFormat
    = YMD -- ^ year-month-day
    | MDY -- ^ month-year-day
    | DMY -- ^ day-month-year
    deriving (Eq, Show)

-- | Flags to tune the behavior of a parser
data Flag
    = MakeRecent           -- ^ Interpret years 0-99 as 1970-2069
    | DefaultToMidnight    -- ^ If no TimeOfDay is supplied for a type where it
                           -- is required, use midnight
    | DefaultToUTC         -- ^ If no timezone is supplied for a type where it
                           -- is required, use UTC
    | RequirePosixUnit     -- ^ Require an 's' at the end of a POSIX timestamp.
                           -- Can be used to distinguish between POSIXTime and
                           -- iso8601 with no separators.
    | AustralianTimeZones  -- ^ Use Australian Timezones
    deriving (Eq,Ord,Show)

data Options =
    Options { formats :: [DateFormat] -- ^ List of what DateFormats to try.
            , seps    :: FastSet      -- ^ Set of accepted separators
            , flags   :: Set.Set Flag -- ^ Set of Flags
            }

-- | A Parser with Options
type OptionedParser a = ReaderT Options Parser a

data DateToken
    = Year Integer  -- ^ An Integer that is known to represent a year
    | Month Integer -- ^ An Integer that is known to represent a month
    | Any Integer   -- ^ An Integer that could represent a day, month, or year
    deriving (Eq, Show)

data ExtendedTimestamp a
    = Timestamp a -- ^ An explicit Timestamp
    | Now         -- ^ The current time
    | Yesterday   -- ^ Midnight yesterday
    | Today       -- ^ Midnight today
    | Tomorrow    -- ^ Midight tomorrow
    deriving (Eq, Show)
