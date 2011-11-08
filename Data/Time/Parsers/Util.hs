{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Data.Time.Parsers.Util ( nDigit
                              , isBCE
                              , onlyParse
                              , defaultOptions
                              , parseWithOptions
                              , parseWithDefaultOptions
                              , isFlagSet
                              , makeBCE
                              , fromExtendedTimestamp
                              , fromExtendedTimestampIO
                              , module Data.Time.Parsers.Types
                              ) where

import Data.Time.Parsers.Types

import Control.Applicative               ((<|>),(<$>))
import Control.Monad.Reader
import Data.Attoparsec.Char8
import Data.Attoparsec.FastSet              (set)
import qualified Data.ByteString.Char8   as B
import Data.Set                          as Set (member, fromList)
import Data.Time

nDigit :: (Read a, Num a) => Int -> Parser a
nDigit n = read <$> count n digit

isBCE :: OptionedParser Bool
isBCE = lift . option False $ const True <$> isBCE'
  where
    isBCE' = skipSpace >> (string "BCE" <|> string "BC")

onlyParse :: OptionedParser a -> OptionedParser a
onlyParse p = p >>= (\r -> lift endOfInput >> return r)

defaultOptions :: Options
defaultOptions = Options { formats = [YMD,DMY,MDY]
                         , seps = set ". /-"
                         , flags = Set.fromList [ MakeRecent
                                                , DefaultToUTC
                                                , DefaultToMidnight
                                                ]
                         }

parseWithOptions :: Options -> OptionedParser a ->
                    B.ByteString -> Result a
parseWithOptions opt p = flip feed B.empty . (parse $ runReaderT p' opt)
  where
    p' = onlyParse p

parseWithDefaultOptions :: OptionedParser a -> B.ByteString -> Result a
parseWithDefaultOptions = parseWithOptions defaultOptions

isFlagSet :: Flag -> OptionedParser Bool
isFlagSet f = asks $ Set.member f . flags

makeBCE :: Monad m => Day -> m Day
makeBCE day = let (y,d,m) = toGregorian day
              in  if (y < 0)
                  then fail "Already BCE"
                  else return $ fromGregorian (negate y + 1) d m

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

fromExtendedTimestampIO :: (FromZonedTime a, ToZonedTime a) =>
                           ExtendedTimestamp a -> IO a
fromExtendedTimestampIO ts = (fromZonedTime <$> getZonedTime) >>=
                             return . flip fromExtendedTimestamp ts