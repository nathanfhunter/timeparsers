{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Time.Parsers.Types where

import Data.Attoparsec.FastSet     (FastSet)
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

data Options = Options { formats             :: [DateFormat]
                       , makeRecent          :: Bool
                       , minDate             :: Maybe Day
                       , maxDate             :: Maybe Day
                       , seps                :: FastSet
                       , allowLeapSeconds    :: Bool
                       , australianTimezones :: Bool
                       }

data DateToken = Year Integer  |
                 Month Integer |
                 Any Integer deriving (Eq, Show)