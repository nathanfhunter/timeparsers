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

import Data.Time.Parsers.Util
import Data.Time.Parsers.Tables        (weekdays, months)

import Control.Applicative             ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8
import Data.Attoparsec.FastSet
import qualified Data.ByteString.Char8 as B
import Data.Char                       (toLower)
import Data.Map                        as M hiding (map)
import Data.Time
import Prelude                         hiding (takeWhile)


lookupMonth :: B.ByteString -> Maybe Integer
lookupMonth = flip M.lookup months . B.map toLower

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

--Date Parsers

parseDateToken :: FastSet -> Parser DateToken
parseDateToken seps' = readDateToken =<< (takeTill $ flip memberChar seps')

skipWeekday :: Parser ()
skipWeekday = option () $
              ( choice $ map stringCI weekdays ) >>
              (option undefined $ char ',')      >>
              skipSpace

yyyymmdd :: OptionedParser Day
yyyymmdd = lift yyyymmdd'

yyyymmdd' :: Parser Day
yyyymmdd' = skipWeekday >>
              (fromGregorianValid <$> nDigit 4 <*> nDigit 2 <*> nDigit 2) >>=
              maybe (fail "Invalid Date Range") return

yymmdd :: OptionedParser Day
yymmdd = isFlagSet MakeRecent >>= lift . yymmdd'

yymmdd' :: Bool -> Parser Day
yymmdd' mr =
    skipWeekday >>
    (fromGregorianValid <$> nDigit 2 <*> nDigit 2 <*> nDigit 2) >>=
    maybe (fail "Invalid Date Range") return'
  where
    return' = if mr then return . forceRecent else return

tokenizedDate :: OptionedParser Day
tokenizedDate = do
    s <- asks seps
    f <- asks formats
    m <- isFlagSet MakeRecent
    lift $ tokenizedDate' s f m

tokenizedDate' :: FastSet -> [DateFormat] -> Bool -> Parser Day
tokenizedDate' seps' formats' makeRecent' = do
    a   <- parseDateToken seps'
    sep <- satisfy $ flip memberChar seps'
    b   <- parseDateToken seps'
    _   <- satisfy (==sep)
    c   <- readDateToken =<< takeWhile isDigit
    let noYear (Year _) = False
        noYear _        = True
        noExplicitYear  = and . map noYear $ [a,b,c]
    date <- tryFormats formats' =<< (return $ makeDate a b c)
    if (makeRecent' && noExplicitYear)
    then return $ forceRecent date
    else return date

fullDate :: OptionedParser Day
fullDate = isFlagSet MakeRecent >>= lift . fullDate'

fullDate' :: Bool -> Parser Day
fullDate' makeRecent' = do
    skipWeekday
    month <- maybe mzero (return . Month) <$>
             lookupMonth =<< (takeWhile isAlpha_ascii)
    _     <- space
    day   <- Any . read . B.unpack <$> takeWhile isDigit
    _     <- string ", "
    year  <- readDateToken =<< takeWhile isDigit
    let forceRecent' = if (noYear year && makeRecent')
                       then forceRecent
                       else id
    forceRecent' <$> makeDate month day year MDY
  where
    noYear (Year _) = False
    noYear _        = True

yearDayOfYear :: OptionedParser Day
yearDayOfYear = do
    s <- asks seps
    lift $ yearDayOfYear' s

yearDayOfYear' :: FastSet -> Parser Day
yearDayOfYear' seps' = do
    year <- nDigit 4
    day  <- maybeSep >> nDigit 3
    yearDayToDate year day
  where
    maybeSep = option () $ satisfy (flip memberChar seps') >> return ()

julianDay :: OptionedParser Day
julianDay = lift julianDay'

julianDay' :: Parser Day
julianDay' = skipWeekday >>
             (string "Julian" <|> string "JD" <|> string "J") >>
             ModifiedJulianDay <$> signed decimal

defaultDay :: OptionedParser Day
defaultDay = do date <- defaultDayCE
                bce  <- isBCE
                if bce then makeBCE date else return date

defaultDayCE :: OptionedParser Day
defaultDayCE = tokenizedDate <|>
               yyyymmdd      <|>
               yearDayOfYear <|>
               yymmdd        <|>
               fullDate      <|>
               julianDay