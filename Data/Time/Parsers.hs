{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Data.Time.Parsers where

import Data.Time.Parsers.Types
import Data.Time.Parsers.Util

import Control.Applicative                  ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8                as A
import Data.Attoparsec.FastSet
import Data.Fixed
import Data.Time                            hiding (parseTime)
import Data.Time.Clock.POSIX                (POSIXTime)
import qualified Data.ByteString.Char8      as B

--Utility Parsers

nDigit :: (Read a, Num a) => Int -> Parser a
nDigit n = read <$> count n digit

parsePico :: Parser Pico
parsePico = (+) <$> (fromInteger <$> decimal) <*> (option 0 postradix)
  where
  postradix = do
    _ <- char '.'
    bs <- A.takeWhile isDigit
    let i = fromInteger . read . B.unpack $ bs
        l = B.length bs
    return (i/10^l)

parseDateToken :: FastSet -> Parser DateToken
parseDateToken seps' = readDateToken =<< (takeTill $ flip memberChar seps')

onlyParse :: ReaderT Options Parser a -> ReaderT Options Parser a
onlyParse p = p >>= (\r -> lift endOfInput >> return r)

--Date Parsers

fourTwoTwo :: ReaderT Options Parser Day
fourTwoTwo = lift fourTwoTwo'

fourTwoTwo' :: Parser Day
fourTwoTwo' = maybe (fail "Invalid Date Range") return =<<
              (fromGregorianValid <$> nDigit 4 <*> nDigit 2 <*> nDigit 2)

twoTwoTwo :: ReaderT Options Parser Day
twoTwoTwo = asks makeRecent >>= lift . twoTwoTwo'

twoTwoTwo' :: Bool -> Parser Day
twoTwoTwo' mr = fmap (if mr then forceRecent else id) $
                maybe (fail "Invalid Date Range") return =<<
                (fromGregorianValid <$> nDigit 2 <*> nDigit 2 <*> nDigit 2)

charSeparated :: ReaderT Options Parser Day
charSeparated = do
    s <- asks seps
    f <- asks formats
    m <- asks makeRecent
    lift $ charSeparated' s f m

charSeparated' :: FastSet -> [DateFormat] -> Bool -> Parser Day
charSeparated' seps' formats' makeRecent' = do
    a   <- parseDateToken seps'
    sep <- satisfy $ flip memberChar seps'
    b   <- parseDateToken seps'
    _   <- satisfy (==sep)
    c   <- readDateToken =<< A.takeWhile isDigit
    let noYear (Year _) = False
        noYear _        = True
        noExplicitYear  = and . map noYear $ [a,b,c]
    date <- tryFormats formats' =<< (return $ makeDate a b c)
    if (makeRecent' && noExplicitYear)
    then return $ forceRecent date
    else return date

fullDate :: ReaderT Options Parser Day
fullDate = asks makeRecent >>= lift . fullDate'

fullDate' :: Bool -> Parser Day
fullDate' makeRecent' = do
    month <- maybe mzero (return . Month) <$>
             lookupMonth =<< (A.takeWhile isAlpha_ascii)
    _ <- space
    day <- Any . read . B.unpack <$> A.takeWhile isDigit
    _ <- string ", "
    year <- readDateToken =<< A.takeWhile isDigit
    let forceRecent' = if (noYear year && makeRecent')
                       then forceRecent
                       else id
    forceRecent' <$> makeDate month day year MDY
  where
    noYear (Year _) = False
    noYear _        = True

yearDayOfYear :: ReaderT Options Parser Day
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

julianDay :: ReaderT Options Parser Day
julianDay = lift julianDay'

julianDay' :: Parser Day
julianDay' = (string "Julian" <|> string "JD" <|> string "J") >>
             ModifiedJulianDay <$> signed decimal

--Time Parsers

twelveHour :: ReaderT Options Parser TimeOfDay
twelveHour = do leapSec <- asks allowLeapSeconds
                th <- lift twelveHour'
                let seconds = timeOfDayToTime th
                if (not leapSec && seconds >= 86400)
                then mzero
                else return th


twelveHour' :: Parser TimeOfDay
twelveHour' = do
    h' <- (nDigit 2 <|> nDigit 1)
    m  <- option 0 $ char ':' >> nDigit 2
    s  <- option 0 $ char ':' >> parsePico
    ampm <- skipSpace >> (string "AM" <|> string "PM")
    h <- case ampm of
      "AM" -> make24 False h'
      "PM" -> make24 True h'
      _    -> fail "Should be impossible."
    maybe (fail "Invalid Time Range") return $
      makeTimeOfDayValid h m s
  where
    make24 pm h = case compare h 12 of
        LT -> return $ if pm then (h+12) else h
        EQ -> return $ if pm then 12 else 0
        GT -> mzero

twentyFourHour :: ReaderT Options Parser TimeOfDay
twentyFourHour = do leapSec <- asks allowLeapSeconds
                    tfh <- lift twentyFourHour'
                    let seconds = timeOfDayToTime tfh
                    if (not leapSec && seconds >= 86400)
                    then mzero
                    else return tfh

twentyFourHour' :: Parser TimeOfDay
twentyFourHour' = maybe (fail "Invalid Time Range") return =<<
                  (colon <|> nocolon)
  where
    colon = makeTimeOfDayValid <$>
            (nDigit 2 <|> nDigit 1) <*>
            (char ':' >> nDigit 2) <*>
            (option 0 $ char ':' >> parsePico)
    nocolon = makeTimeOfDayValid <$>
              nDigit 2 <*>
              option 0 (nDigit 2) <*>
              option 0 parsePico

--TimeZone Parsers

offsetTimezone :: ReaderT Options Parser TimeZone
offsetTimezone = lift offsetTimezone'

offsetTimezone' :: Parser TimeZone
offsetTimezone' =  (char 'Z' >> return utc) <|>
                   ((plus <|> minus) <*> timezone'')
  where
    plus  = char '+' >> return minutesToTimeZone
    minus = char '-' >> return (minutesToTimeZone . negate)
    hour p = p >>= (\n -> if (n < 12) then (return $ 60*n) else mzero)
    minute  p = option () (char ':' >> return ()) >> p >>=
                (\n -> if (n < 60) then return n else mzero)
    timezone'' = choice [ (+) <$> (hour $ nDigit 2) <*> (minute $ nDigit 2)
                        , (+) <$> (hour $ nDigit 1) <*> (minute $ nDigit 2)
                        , hour $ nDigit 2
                        , hour $ nDigit 1
                        ]

namedTimezone :: ReaderT Options Parser TimeZone
namedTimezone = asks australianTimezones >>= lift . namedTimezone'

namedTimezone' :: Bool -> Parser TimeZone
namedTimezone' aussie = (lookup' <$> A.takeWhile isAlpha_ascii) >>=
                        maybe (fail "Invalid Timezone") return
  where
    lookup' = if aussie then lookupAusTimezone else lookupTimezone

--Timestamp Parsers

posixTime :: ReaderT Options Parser POSIXTime
posixTime = asks requirePosixUnits >>= lift . posixTime'

posixTime' :: Bool -> Parser POSIXTime
posixTime' requireS = realToFrac . readDouble . B.unpack <$>
                      if requireS then posixWithS else posixTime''
  where
    posixTime'' = (B.append) <$>
                  (option "0" $ takeWhile1 isDigit) <*>
                  (option "" $ (B.cons) <$> char '.' <*> A.takeWhile isDigit)
    posixWithS  = posixTime''>>=(\r -> char 's' >> return r)
    readDouble :: String -> Double
    readDouble = read

zonedTime :: ReaderT Options Parser Day ->
             ReaderT Options Parser TimeOfDay ->
             ReaderT Options Parser TimeZone ->
             ReaderT Options Parser ZonedTime
zonedTime date time timezone =
    ZonedTime <$>
    localTime date time <*>
    (maybeSpace >> timezone)
  where
    maybeSpace = option undefined $ lift space

localTime :: ReaderT Options Parser Day ->
             ReaderT Options Parser TimeOfDay ->
             ReaderT Options Parser LocalTime
localTime date time = asks defaultToMidnight >>= localTime'
  where
    localTime' b = LocalTime <$> date <*> time' b
    time' b = timeSep >> if b then (option midnight time) else time
    timeSep = lift $ char 'T' <|> space

timestamp :: FromTimeStamp a =>
             ReaderT Options Parser Day ->
             ReaderT Options Parser TimeOfDay ->
             ReaderT Options Parser TimeZone ->
             ReaderT Options Parser a
timestamp date time timezone = choice [ fromTimeStamp . Zoned <$> zonedtime
                                      , fromTimeStamp . Local <$> localtime
                                      , fromTimeStamp . Local <$> day
                                      ]
  where
    zonedtime = ZonedTime <$> localtime <*> timezone'
    localtime = LocalTime <$> date <*> time'
    day       = flip LocalTime midnight <$> date
    time'     = lift (space <|> char 'T') >> time
    timezone' = lift (option undefined space) >> timezone

--Defaults and Debugging

defaultOptions :: Options
defaultOptions = Options { formats = [YMD,DMY,MDY]
                         , makeRecent = True
                         , minDate = Nothing
                         , maxDate = Nothing
                         , seps = (set ". /-")
                         , allowLeapSeconds = False
                         , defaultToMidnight = True
                         , requirePosixUnits = False
                         , australianTimezones = False
                         }

defaultDate :: ReaderT Options Parser Day
defaultDate = charSeparated <|>
              fourTwoTwo    <|>
              yearDayOfYear <|>
              twoTwoTwo     <|>
              fullDate      <|>
              julianDay

defaultTime :: ReaderT Options Parser TimeOfDay
defaultTime = twelveHour <|> twentyFourHour

defaultTimeZone :: ReaderT Options Parser TimeZone
defaultTimeZone = offsetTimezone <|> namedTimezone

defaultTimeStamp :: FromTimeStamp a => ReaderT Options Parser a
defaultTimeStamp = onlyParse timestamp' <|>
                   (fromTimeStamp . Posix <$>  posixTime)
  where
    timestamp' = timestamp defaultDate defaultTime defaultTimeZone

debugParse :: Options -> ReaderT Options Parser a ->
              B.ByteString -> Result a
debugParse opt p = flip feed B.empty . parse (runReaderT p opt)