{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Data.Time.Parsers.Util ( nDigit
                              , isBCE
                              , onlyParse
                              , defaultOptions
                              , withOptions
                              , withDefaultOptions
                              , parseWithOptions
                              , parseWithDefaultOptions
                              , isFlagSet
                              , makeBCE
                              , fromExtendedTimestamp
                              , fromExtendedTimestampIO
                              , module Data.Time.Parsers.Types
                              ) where

import Data.Time.Parsers.Types

import Control.Applicative               ((<|>),(<$>),(<*),(*>))
import Control.Monad.Reader
import Data.Attoparsec.Char8
import Data.Attoparsec.FastSet              (set)
import qualified Data.ByteString.Char8   as B
import Data.Set                          as Set (member, fromList)
import Data.Time

-- | Parse a given number of digits
nDigit :: (Read a, Num a) => Int -> Parser a
nDigit n = read <$> count n digit

-- | Return true if the strings "BC" or "BCE" are consumed, false otherwise
isBCE :: OptionedParser Bool
isBCE = lift . option False $ const True <$> isBCE'
  where
    isBCE' = skipSpace *> (string "BCE" <|> string "BC")

-- | Fail if the given parser fails to consume all of the input
onlyParse :: OptionedParser a -> OptionedParser a
onlyParse p = p <* lift endOfInput

-- | Default Options to use:
-- Try YMD, then MDY, then DMY
-- Accept '.', ' ', '/', '-' as separators.
-- Use flags MakeRecent, DefaultToUTC, DefaultToMidnight
defaultOptions :: Options
defaultOptions = Options { formats = [YMD,MDY,DMY]
                         , seps = set ". /-"
                         , flags = Set.fromList [ MakeRecent
                                                , DefaultToUTC
                                                , DefaultToMidnight
                                                ]
                         }

withOptions :: OptionedParser a -> Options -> Parser a
withOptions = runReaderT

withDefaultOptions :: OptionedParser a -> Parser a
withDefaultOptions = flip runReaderT defaultOptions

-- | Use given options and parser to parse a single Timestamp.
-- always feeds empty, so a Partial result is never returned.
-- Ignores preceding and trailing whitespace.
parseWithOptions :: Options -> OptionedParser a ->
                    B.ByteString -> Result a
parseWithOptions opt p = flip feed B.empty . (parse $ runReaderT p' opt)
  where
    p' = onlyParse (lift skipSpace *> p <* lift skipSpace)

-- | Use default options to parse single Timestamp with a given parser,
-- ignoring preceding and trailing whitespace
parseWithDefaultOptions :: OptionedParser a -> B.ByteString -> Result a
parseWithDefaultOptions = parseWithOptions defaultOptions

-- | Return whether a given flag is set.
isFlagSet :: Flag -> OptionedParser Bool
isFlagSet f = asks $ Set.member f . flags

-- | Converts a CE date into a BCE date. Fails if the date is already BCE
-- Warning: If you anticipate BCE dates, it is advisable to not use the
-- MakeRecent flag. It will cause ByteStrings such as "79 BC" to be parsed as
-- "1979 BCE"
makeBCE :: Monad m => Day -> m Day
makeBCE day = let (y,d,m) = toGregorian day
              in  if (y < 0)
                  then fail "Already BCE"
                  else return $ fromGregorian (negate y + 1) d m

-- | Given a timestamp to use as the current time, purely convert an
-- ExtendedTimestamp to a timestamp
fromExtendedTimestamp :: (FromZonedTime a, ToZonedTime a) =>
                         a -> ExtendedTimestamp a -> a
fromExtendedTimestamp now ts = case ts of
    Timestamp a -> a
    Now         -> now
    Yesterday   -> fromZonedTime . addDays' (-1) . atMidnight $ toZonedTime now
    Today       -> fromZonedTime . atMidnight $ toZonedTime now
    Tomorrow    -> fromZonedTime . addDays' 1 . atMidnight $ toZonedTime now
  where
    atMidnight (ZonedTime (LocalTime d _) tz) =
        ZonedTime (LocalTime d midnight) tz
    addDays' n (ZonedTime (LocalTime d tod) tz) =
        ZonedTime (LocalTime (addDays n d) tod) tz

-- | Use getZonedTime to get the current time, and use it to convert an
-- ExtendedTimestamp to a timestamp
fromExtendedTimestampIO :: (FromZonedTime a, ToZonedTime a) =>
                           ExtendedTimestamp a -> IO a
fromExtendedTimestampIO ts = (fromZonedTime <$> getZonedTime) >>=
                             return . flip fromExtendedTimestamp ts