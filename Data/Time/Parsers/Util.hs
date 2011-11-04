{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Data.Time.Parsers.Util where

import Data.Time.Parsers.Tables
import Data.Time.Parsers.Types

import Control.Applicative               ((<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8             (isDigit)
import qualified Data.ByteString.Char8   as B
import Data.Char                         (toLower, toUpper)
import Data.Convertible                  (convert)
import Data.Convertible.Instances()
import Data.Map                          as M
import Data.Set                          as Set (member)
import Data.Time.Calendar
import Data.Time.Clock.POSIX             (POSIXTime)
import Data.Time.LocalTime               (TimeZone, ZonedTime)

lookupMonth :: B.ByteString -> Maybe Integer
lookupMonth = flip M.lookup months . B.map toLower

lookupTimezone :: B.ByteString -> Maybe TimeZone
lookupTimezone = flip M.lookup timezones . B.map toUpper

lookupAusTimezone :: B.ByteString -> Maybe TimeZone
lookupAusTimezone bs = M.lookup (B.map toUpper bs) ausTimezones  <|>
                       lookupTimezone bs

readDateToken :: forall (m :: * -> *). Monad m => B.ByteString -> m DateToken
readDateToken bs |notDigits bs     = maybe (fail $ "Invalid DateToken" ++
                                                   (B.unpack bs))
                                           (return . Month)
                                           (lookupMonth bs)
                 |B.length bs >= 3 = returnRead Year bs
                 |otherwise        = returnRead Any $ bs
  where
    returnRead f = return . f . read . B.unpack
    notDigits = not . B.all isDigit

makeDate :: forall (m :: * -> *). Monad m =>
            DateToken -> DateToken -> DateToken -> DateFormat -> m Day
makeDate a b c f = case (a, b, c) of
    (Year y, m, Any d) -> ymd y m d f
    (Month m, Any d, y) -> mdy m d y f
    (Any d, Month m, Year y) -> if   (f==DMY)
                                then (makeDate' y m d)
                                else fail'
    (Any p, Month m, Any q) -> case f of
        YMD -> makeDate' p m q
        MDY -> fail'
        DMY -> makeDate' q m p
    (Any p, Any q, Year y) -> case f of
        YMD -> fail'
        MDY -> makeDate' y p q
        DMY -> makeDate' y q p
    (Any p, Any q, Any r) -> case f of
        YMD -> makeDate' p q r
        MDY -> makeDate' r p q
        DMY -> makeDate' r q p
    _ -> fail'
  where
    ymd y (Month m) d YMD = makeDate' y m d
    ymd y (Any m)   d YMD = makeDate' y m d
    ymd _ _         _ _   = fail'
    mdy m d (Year y) MDY = makeDate' y m d
    mdy m d (Any y)  MDY = makeDate' y m d
    mdy _ _ _        _   = fail'
    fail' = fail "Unsupported Date Format"
    makeDate' y m d = if validDate y m d
                      then return $ fromGregorian' y m d
                      else fail "Invalid date range"
    validDate y m' d' = let m = fromIntegral m'
                            d = fromIntegral d'
                        in  and [ m > 0
                                , d > 0
                                , m <= 12
                                , d <= (gregorianMonthLength y m)
                                ]
    fromGregorian' y m d = fromGregorian y (fromIntegral m) (fromIntegral d)

forceRecent :: Day -> Day
forceRecent day | y < 100 && y < 70  = addGregorianYearsClip 2000 day
                | y < 100            = addGregorianYearsClip 1900 day
                | otherwise          = day
  where
    (y,_,_) = toGregorian day

tryFormats :: forall (m :: * -> *). MonadPlus m =>
              [DateFormat] -> (DateFormat -> m Day) -> m Day
tryFormats fs d = (msum $ Prelude.map d fs)

yearDayToDate :: forall (m:: * -> *). Monad m =>
                 Integer -> Integer -> m Day
yearDayToDate year day = if (day <= lastDay && day > 0)
                         then return . addDays (day - 1) $
                              fromGregorian year 0 0
                         else fail "Invalid Day of Year"
  where
    lastDay = if isLeapYear year then 366 else 365

isFlagSet :: Flag -> OptionedParser Bool
isFlagSet f = asks $ Set.member f . flags

posixToZoned :: POSIXTime -> ZonedTime
posixToZoned = convert

makeBCE :: Monad m => Day -> m Day
makeBCE day = let (y,d,m) = toGregorian day
              in  if (y < 0)
                  then fail "Already BCE"
                  else return $ fromGregorian (negate y + 1) d m