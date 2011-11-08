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


data DateFormat = YMD | MDY | DMY deriving (Eq, Show)

data Flag = MakeRecent          |
            DefaultToMidnight   |
            DefaultToUTC        |
            RequirePosixUnit    |
            AustralianTimeZones deriving (Eq,Ord,Show)

data Options = Options { formats :: [DateFormat]
                       , seps    :: FastSet
                       , flags   :: Set.Set Flag
                       }

type OptionedParser a = ReaderT Options Parser a

data DateToken = Year Integer  |
                 Month Integer |
                 Any Integer deriving (Eq, Show)

data ExtendedTimestamp a = Timestamp a |
                           Now         |
                           Yesterday   |
                           Today       |
                           Tomorrow    deriving (Eq, Show)
