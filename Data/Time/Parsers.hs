{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Data.Time.Parsers where

import Data.Time.Parsers.Types
import Data.Time.Parsers.Util

import Control.Applicative                  ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8                as A
import Data.Attoparsec.FastSet              (set, FastSet, memberChar)
import Data.Fixed                           (Pico)
import Data.Set                             (fromList)
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

onlyParse :: DateParser a -> DateParser a
onlyParse p = p >>= (\r -> lift endOfInput >> return r)

--Date Parsers

fourTwoTwo :: DateParser Day
fourTwoTwo = lift fourTwoTwo'

fourTwoTwo' :: Parser Day
fourTwoTwo' = maybe (fail "Invalid Date Range") return =<<
              (fromGregorianValid <$> nDigit 4 <*> nDigit 2 <*> nDigit 2)

twoTwoTwo :: DateParser Day
twoTwoTwo = isFlagSet MakeRecent >>= lift . twoTwoTwo'

twoTwoTwo' :: Bool -> Parser Day
twoTwoTwo' mr = fmap (if mr then forceRecent else id) $
                maybe (fail "Invalid Date Range") return =<<
                (fromGregorianValid <$> nDigit 2 <*> nDigit 2 <*> nDigit 2)

charSeparated :: DateParser Day
charSeparated = do
    s <- asks seps
    f <- asks formats
    m <- isFlagSet MakeRecent
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

fullDate :: DateParser Day
fullDate = isFlagSet MakeRecent >>= lift . fullDate'

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

yearDayOfYear :: DateParser Day
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

julianDay :: DateParser Day
julianDay = lift julianDay'

julianDay' :: Parser Day
julianDay' = (string "Julian" <|> string "JD" <|> string "J") >>
             ModifiedJulianDay <$> signed decimal

--Time Parsers

twelveHour :: DateParser TimeOfDay
twelveHour = do leapSec <- isFlagSet AllowLeapSeconds
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

twentyFourHour :: DateParser TimeOfDay
twentyFourHour = do leapSec <- isFlagSet AllowLeapSeconds
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

offsetTimezone :: DateParser TimeZone
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

namedTimezone :: DateParser TimeZone
namedTimezone = isFlagSet AustralianTimezones >>= lift . namedTimezone'

namedTimezone' :: Bool -> Parser TimeZone
namedTimezone' aussie = (lookup' <$> A.takeWhile isAlpha_ascii) >>=
                        maybe (fail "Invalid Timezone") return
  where
    lookup' = if aussie then lookupAusTimezone else lookupTimezone

--Timestamp Parsers

posixTime :: DateParser POSIXTime
posixTime = isFlagSet RequirePosixUnit >>= lift . posixTime'

posixTime' :: Bool -> Parser POSIXTime
posixTime' requireS = do
    bytestring <- (B.append) <$>
                  (option "0" $ takeWhile1 isDigit) <*>
                  (option "" $ (B.cons) <$> char '.' <*> A.takeWhile isDigit)
    when requireS $ char 's' >> return ()
    return . realToFrac . readDouble . B.unpack $ bytestring
  where
    readDouble :: String -> Double
    readDouble = read

zonedTime :: DateParser Day -> DateParser TimeOfDay ->
             DateParser TimeZone -> DateParser ZonedTime
zonedTime date time timezone = do
    defaultToUTC <- isFlagSet DefaultToUTC
    let timezone'  = (option undefined $ lift space) >> timezone
        mtimezone  = if defaultToUTC
                     then (option utc timezone')
                     else timezone'
    ZonedTime <$> localTime date time <*> mtimezone

localTime :: DateParser Day -> DateParser TimeOfDay -> DateParser LocalTime
localTime date time = do
    defaultToMidnight <- isFlagSet DefaultToMidnight
    let time' = (lift $ char 'T' <|> space) >> time
        mtime = if defaultToMidnight
                then (option midnight time')
                else time'
    LocalTime <$> date <*> mtime

anyFromZoned :: FromZonedTime a =>
                DateParser Day -> DateParser TimeOfDay ->
                DateParser TimeZone -> DateParser a
anyFromZoned d t tz = fromZonedTime <$> zonedTime d t tz

anyFromPosix :: FromZonedTime a => DateParser a
anyFromPosix = fromZonedTime . posixToZoned <$> posixTime

--Defaults and Debugging

defaultOptions :: Options
defaultOptions = Options { formats = [YMD,DMY,MDY]
                         , seps = (set ". /-")
                         , flags = fromList [ MakeRecent
                                            , DefaultToUTC
                                            , DefaultToMidnight
                                            ]
                         }

defaultDate :: DateParser Day
defaultDate = charSeparated <|>
              fourTwoTwo    <|>
              yearDayOfYear <|>
              twoTwoTwo     <|>
              fullDate      <|>
              julianDay

defaultTime :: DateParser TimeOfDay
defaultTime = twelveHour <|> twentyFourHour

defaultTimeZone :: DateParser TimeZone
defaultTimeZone = offsetTimezone <|> namedTimezone

defaultTimeStamp :: FromZonedTime a => DateParser a
defaultTimeStamp = onlyParse anyFromZoned' <|> onlyParse anyFromPosix
  where
    anyFromZoned' = anyFromZoned defaultDate defaultTime defaultTimeZone

debugParse :: Options -> DateParser a ->
              B.ByteString -> Result a
debugParse opt p = flip feed B.empty . parse (runReaderT p opt)