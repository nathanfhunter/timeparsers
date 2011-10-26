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

data Flag = MakeRecent          |
            AllowLeapSeconds    |
            DefaultToMidnight   |
            DefaultToUTC        |
            RequirePosixUnit    |
            AustralianTimezones deriving (Eq,Ord,Show)

data Options = Options { formats :: [DateFormat]
                       , seps    :: FastSet
                       , flags   :: Set.Set Flag
                       }

type DateParser a = ReaderT Options Parser a

data DateToken = Year Integer  |
                 Month Integer |
                 Any Integer deriving (Eq, Show)