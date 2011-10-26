{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Time.Parsers.Types where

import Data.Attoparsec.FastSet     (FastSet)
import Data.Convertible
import Data.Convertible.Instances()
import Data.Time
import Data.Time.Clock.POSIX

data TimeStamp = Zoned ZonedTime |
                 Local LocalTime |
                 Posix POSIXTime deriving (Show)

class FromTimeStamp a where
    fromTimeStamp :: TimeStamp -> a

instance FromTimeStamp LocalTime where
    fromTimeStamp timestamp = case timestamp of
        Zoned z -> zonedTimeToLocalTime z
        Local l -> l
        Posix p -> zonedTimeToLocalTime $ convert p
instance FromTimeStamp UTCTime where
    fromTimeStamp timestamp = case timestamp of
        Zoned z -> zonedTimeToUTC z
        Local l -> localTimeToUTC utc l
        Posix p -> convert p
instance FromTimeStamp ZonedTime where
    fromTimeStamp timestamp = case timestamp of
        Zoned z -> z
        Local l -> ZonedTime l utc
        Posix p -> convert p
instance FromTimeStamp Day where
    fromTimeStamp timestamp = case timestamp of
        Zoned z -> localDay . zonedTimeToLocalTime $ z
        Local l -> localDay l
        Posix p -> utctDay $ convert p
instance FromTimeStamp POSIXTime where
    fromTimeStamp timestamp = case timestamp of
        Zoned z -> convert z
        Local l -> convert . localTimeToUTC utc $ l
        Posix p -> p

data DateFormat = YMD | MDY | DMY deriving (Eq, Show)

data Options = Options { formats             :: [DateFormat]
                       , makeRecent          :: Bool
                       , minDate             :: Maybe Day
                       , maxDate             :: Maybe Day
                       , seps                :: FastSet
                       , allowLeapSeconds    :: Bool
                       , defaultToMidnight   :: Bool
                       , requirePosixUnits   :: Bool
                       , australianTimezones :: Bool
                       }

data DateToken = Year Integer  |
                 Month Integer |
                 Any Integer deriving (Eq, Show)