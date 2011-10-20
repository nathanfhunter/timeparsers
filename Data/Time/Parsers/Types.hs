{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Time.Parsers.Types where

import Data.Convertible
import Data.Convertible.Instances()
import Data.Time
import Data.Time.Clock.POSIX

data DateTime = UTC UTCTime   |
                ZT  ZonedTime |
                PT  POSIXTime |
                D   Day deriving (Show)

instance Convertible DateTime UTCTime where
    safeConvert dt = case dt of
        UTC u -> return u
        ZT  z -> safeConvert z
        PT  p -> safeConvert p
        D   d -> return $ UTCTime d 0
instance Convertible DateTime ZonedTime where
    safeConvert dt = case dt of
        UTC u -> safeConvert u
        ZT  z -> return z
        PT  p -> safeConvert p
        D   d -> safeConvert $ UTCTime d 0
instance Convertible DateTime POSIXTime where
    safeConvert dt = case dt of
        UTC u -> safeConvert u
        ZT  z -> safeConvert z
        PT  p -> return p
        D   d -> safeConvert $ UTCTime d 0

data DateFormat = YMD | MDY | DMY deriving (Eq, Show)

data Dates m = Dates { ymd :: m Day
                     , mdy :: m Day
                     , dmy :: m Day
                     }
deriving instance Show (m Day) => Show (Dates m)
deriving instance Eq (m Day) => Eq (Dates m)

data Options = Options { strict     :: Bool
                       , format     :: DateFormat
                       , makeRecent :: Bool
                       , minDate    :: Day
                       , maxDate    :: Day
                       }