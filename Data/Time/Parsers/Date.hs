{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Data.Time.Parsers.Date ( yyyymmdd
                              , yymmdd
                              , tokenizedDate
                              , fullDate
                              , yearDayOfYear
                              , julianDay
                              , defaultDay
                              , defaultDayCE
                              ) where

import           Data.Time.Parsers.Util
import           Data.Time.Parsers.Tables (weekdays, months)

import           Control.Applicative      ((<$>),(<*>),(<|>))
import           Control.Monad.Reader
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8    as B
import           Data.Char                (toLower)
import           Data.Map                 as M hiding (map)
import           Data.Time
import           Prelude                  hiding (takeWhile)


lookupMonth :: B.ByteString -> Maybe Integer
lookupMonth = flip M.lookup months . B.map toLower

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

--Date Parsers

skipWeekday :: Parser ()
skipWeekday = option () $
              ( choice $ map stringCI weekdays ) >>
              (option undefined $ char ',')      >>
              skipSpace

-- | parse a date with no separators of the format yyyymmdd.
-- Will treat a preceding weekday as noise.
yyyymmdd :: OptionedParser Day
yyyymmdd = lift yyyymmdd'

yyyymmdd' :: Parser Day
yyyymmdd' = skipWeekday >>
              (fromGregorianValid <$> nDigit 4 <*> nDigit 2 <*> nDigit 2) >>=
              maybe (fail "Invalid Date Range") return

-- | parse a date with no separators of the format yymmdd.
-- Will treat a preceding weekday as noise
yymmdd :: OptionedParser Day
yymmdd = isFlagSet MakeRecent >>= lift . yymmdd'

yymmdd' :: Bool -> Parser Day
yymmdd' mr =
    skipWeekday >>
    (fromGregorianValid <$> nDigit 2 <*> nDigit 2 <*> nDigit 2) >>=
    maybe (fail "Invalid Date Range") return'
  where
    return' = if mr then return . forceRecent else return

-- | parse a date formatted as three values separated by some separator
-- values can be month names, abbreviations, or numeric values. Numeric values
-- with more than two digits are assumed to represent years.

numericDateToken :: Parser DateToken
numericDateToken = tokenize <$> takeWhile1 isDigit
  where
    tokenize bs = if   B.length bs > 2
                  then Year . read $ B.unpack bs
                  else Any  . read $ B.unpack bs

namedMonthToken :: Parser DateToken
namedMonthToken = (lookupMonth <$> takeWhile isAlpha_ascii) >>=
                  maybe (fail "Invalid Month") (return . Month)

dateToken :: Parser DateToken
dateToken = numericDateToken <|> namedMonthToken

tokenizedDate :: OptionedParser Day
tokenizedDate = do
    s <- asks seps
    f <- asks formats
    m <- isFlagSet MakeRecent
    lift $ tokenizedDate' s f m

tokenizedDate' :: String -> [DateFormat] -> Bool -> Parser Day
tokenizedDate' seps' formats' makeRecent' = do
    a   <- dateToken
    sep <- satisfy $ inClass seps'
    b   <- dateToken
    _   <- satisfy (==sep)
    c   <- numericDateToken
    let noYear (Year _) = False
        noYear _        = True
        noExplicitYear  = and . map noYear $ [a,b,c]
    date <- tryFormats formats' =<< (return $ makeDate a b c)
    if (makeRecent' && noExplicitYear)
    then return $ forceRecent date
    else return date


-- | parse a date such as "January 1, 2011".
-- Will treat a preceding weekday as noise
fullDate :: OptionedParser Day
fullDate = isFlagSet MakeRecent >>= lift . fullDate'

fullDate' :: Bool -> Parser Day
fullDate' makeRecent' = do
    skipWeekday
    month <- maybe mzero (return . Month) <$>
             lookupMonth =<< (takeWhile isAlpha_ascii)
    _     <- space
    day   <- numericDateToken
    _     <- string ", "
    year  <- numericDateToken
    let forceRecent' = if (noYear year && makeRecent')
                       then forceRecent
                       else id
    forceRecent' <$> makeDate month day year MDY
  where
    noYear (Year _) = False
    noYear _        = True
-- | parse a date in year, day of year format
-- i.e yyyy/ddd or yyyydd
yearDayOfYear :: OptionedParser Day
yearDayOfYear = do
    s <- asks seps
    lift $ yearDayOfYear' s

yearDayOfYear' :: String -> Parser Day
yearDayOfYear' seps' = do
    year <- nDigit 4
    day  <- maybeSep >> nDigit 3
    yearDayToDate year day
  where
    maybeSep = option () $ satisfy (inClass seps') >> return ()

-- | parse a julian day (days since 4713/1/1 BCE)
-- Must prepend with "J", "JD", or "Julian"
julianDay :: OptionedParser Day
julianDay = lift julianDay'

julianDay' :: Parser Day
julianDay' = skipWeekday >>
             (string "Julian" <|> string "JD" <|> string "J") >>
             julianDay'' <$> signed decimal
  where
    julianDay'' n = ModifiedJulianDay $ n - 2399963

-- | parse a date using tokenizedDate, yyyymmdd, yymmdd, yearDayOfYear, fullDate
-- or julianDay, converting to BCE if necessary
defaultDay :: OptionedParser Day
defaultDay = do date <- defaultDayCE
                bce  <- isBCE
                if bce then makeBCE date else return date

-- | Parse a date as in defaultDay, but don't check for BCE
defaultDayCE :: OptionedParser Day
defaultDayCE = tokenizedDate <|>
               yyyymmdd      <|>
               yearDayOfYear <|>
               yymmdd        <|>
               fullDate      <|>
               julianDay
