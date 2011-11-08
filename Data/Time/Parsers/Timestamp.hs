{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Parsers.Timestamp ( offsetTimeZone
                                   , namedTimeZone
                                   , defaultTimeZone
                                   , posixTime
                                   , zonedTime
                                   , localTime
                                   , defaultZonedTime
                                   , defaultLocalTime
                                   , defaultTimestamp
                                   , extendTimestamp
                                   ) where

import Data.Time.Parsers.Util
import Data.Time.Parsers.Date
import Data.Time.Parsers.Tables        (timeZones, ausTimeZones)
import Data.Time.Parsers.Time

import Control.Applicative             ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import Data.Char                       (toUpper)
import Data.Map                        as M
import Data.Time
import Data.Time.Clock.POSIX
import Prelude                         hiding (takeWhile)


lookupTimeZone :: B.ByteString -> Maybe TimeZone
lookupTimeZone = flip M.lookup timeZones . B.map toUpper

lookupAusTimeZone :: B.ByteString -> Maybe TimeZone
lookupAusTimeZone bs = M.lookup (B.map toUpper bs) ausTimeZones  <|>
                       lookupTimeZone bs

--TimeZone Parsers

offsetTimeZone :: OptionedParser TimeZone
offsetTimeZone = lift offsetTimeZone'

offsetTimeZone' :: Parser TimeZone
offsetTimeZone' =  (char 'Z' >> return utc) <|>
                   ((plus <|> minus) <*> timeZone'')
  where
    plus  = char '+' >> return minutesToTimeZone
    minus = char '-' >> return (minutesToTimeZone . negate)
    hour p = p >>= (\n -> if (n < 24) then (return $ 60*n) else mzero)
    minute  p = option () (char ':' >> return ()) >> p >>=
                (\n -> if (n < 60) then return n else mzero)
    timeZone'' = choice [ (+) <$> (hour $ nDigit 2) <*> (minute $ nDigit 2)
                        , (+) <$> (hour $ nDigit 1) <*> (minute $ nDigit 2)
                        , hour $ nDigit 2
                        , hour $ nDigit 1
                        ]

namedTimeZone :: OptionedParser TimeZone
namedTimeZone = isFlagSet AustralianTimeZones >>= lift . namedTimeZone'

namedTimeZone' :: Bool -> Parser TimeZone
namedTimeZone' aussie = (lookup' <$> takeWhile isAlpha_ascii) >>=
                        maybe (fail "Invalid TimeZone") return
  where
    lookup' = if aussie then lookupAusTimeZone else lookupTimeZone

--Timestamp Parsers

posixTime :: OptionedParser POSIXTime
posixTime = isFlagSet RequirePosixUnit >>= lift . posixTime'

posixTime' :: Bool -> Parser POSIXTime
posixTime' requireS = do
    r <- rational
    when requireS $ char 's' >> return ()
    return r

zonedTime :: OptionedParser LocalTime ->
             OptionedParser TimeZone ->
             OptionedParser ZonedTime
zonedTime localT timeZone = do
    defaultToUTC <- isFlagSet DefaultToUTC
    let timeZone'  = (option undefined $ lift space) >> timeZone
        mtimeZone  = if defaultToUTC
                     then (option utc timeZone')
                     else timeZone'
    zonedT <- ZonedTime <$> localT <*> mtimeZone
    bce <- isBCE
    if bce then makeBCE' zonedT else return zonedT
  where
    makeBCE' (ZonedTime (LocalTime d t) tz) =
        makeBCE d >>= \d' -> return $ ZonedTime (LocalTime d' t) tz

localTime :: OptionedParser Day ->
             OptionedParser TimeOfDay ->
             OptionedParser LocalTime
localTime date time = do
    defaultToMidnight <- isFlagSet DefaultToMidnight
    let time' = (lift $ char 'T' <|> space) >> time
        mtime = if defaultToMidnight
                then (option midnight time')
                else time'
    localT <- LocalTime <$> date <*> mtime
    bce <- isBCE
    if bce then makeBCE' localT else return localT
  where
    makeBCE' (LocalTime d t) = makeBCE d >>= \d' -> return $ LocalTime d' t

extendTimestamp :: FromZonedTime a =>
                   OptionedParser a ->
                   OptionedParser ( ExtendedTimestamp a )
extendTimestamp p = either Timestamp id <$> eitherP p extendedTimestamp'
  where
    extendedTimestamp' =
      lift $ choice [ stringCI "now"       >> return Now
                    , stringCI "yesterday" >> return Yesterday
                    , stringCI "today"     >> return Today
                    , stringCI "tomorrow"  >> return Tomorrow
                    ]

defaultTimeZone :: OptionedParser TimeZone
defaultTimeZone = namedTimeZone <|> offsetTimeZone

defaultLocalTime :: OptionedParser LocalTime
defaultLocalTime = localTime defaultDayCE defaultTimeOfDay

defaultZonedTime :: OptionedParser ZonedTime
defaultZonedTime = zonedTime defaultLocalTime defaultTimeZone

defaultTimestamp :: FromZonedTime a => OptionedParser a
defaultTimestamp = fromZonedTime <$> defaultTimestamp'
  where
    defaultTimestamp' = onlyParse defaultZonedTime <|>
                        (toZonedTime <$> posixTime)