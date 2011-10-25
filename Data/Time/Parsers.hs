{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Parsers where

import Data.Time.Parsers.Types
import Data.Time.Parsers.Util

import Control.Applicative                  ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8                as A
import Data.Attoparsec.FastSet
import Data.Fixed
import Data.Time                            hiding (parseTime)
import qualified Data.ByteString.Char8      as B

count2 :: Parser DateToken
count2 = Any . read <$> count 2 digit

count4 :: Parser DateToken
count4 = Year . read <$> count 4 digit

parseDateToken :: FastSet -> Parser DateToken
parseDateToken seps' = readDateToken =<< (takeTill $ flip memberChar seps')

fourTwoTwo :: ReaderT Options Parser Day
fourTwoTwo = lift fourTwoTwo'

fourTwoTwo' :: Parser Day
fourTwoTwo' = ($ YMD) =<< (makeDate <$> count4 <*> count2 <*> count2)

twoTwoFour :: ReaderT Options Parser Day
twoTwoFour = (asks formats) >>= (lift . twoTwoFour')

twoTwoFour' :: [DateFormat] -> Parser Day
twoTwoFour' fs = tryFormats fs =<<
                 (makeDate <$> count2 <*> count2 <*> count4)

twoTwoTwo :: ReaderT Options Parser Day
twoTwoTwo = (asks formats) >>= (lift . twoTwoTwo')

twoTwoTwo' :: [DateFormat] -> Parser Day
twoTwoTwo' fs = tryFormats fs =<<
                (makeDate <$> count2 <*> count2 <*> count2)

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

yearDayOfYear :: ReaderT Options Parser Day
yearDayOfYear = do
    s <- asks seps
    lift $ yearDayOfYear' s

yearDayOfYear' :: FastSet -> Parser Day
yearDayOfYear' seps' = do
    year <- read <$> count 4 digit
    day  <- fmap read $ maybeSep >> count 3 digit
    yearDayToDate year day
  where
    maybeSep = option () $ satisfy (flip memberChar seps') >> return ()

julianDay :: ReaderT Options Parser Day
julianDay = lift julianDay'

julianDay' :: Parser Day
julianDay' = (string "Julian" <|> string "JD" <|> string "J") >>
             ModifiedJulianDay <$> signed decimal

fullDate :: ReaderT Options Parser Day
fullDate = asks makeRecent >>= lift . fullDate'

fullDate' :: Bool -> Parser Day
fullDate' makeRecent' = do
    month <- maybe mzero (return . Month) <$>
             namedMonth =<< (A.takeWhile isAlpha_ascii)
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

parsePico :: Parser Pico
parsePico = (+) <$> (fromInteger <$> decimal) <*> (option 0 postradix)
  where
  postradix = do
    _ <- char '.'
    bs <- A.takeWhile isDigit
    let i = fromInteger . read . B.unpack $ bs
        l = B.length bs
    return (i/10^l)

twelveHour :: ReaderT Options Parser TimeOfDay
twelveHour = do leapSec <- asks allowLeapSeconds
                th <- lift twelveHour'
                let seconds = timeOfDayToTime th
                if (not leapSec && seconds >= 86400)
                then mzero
                else return th


twelveHour' :: Parser TimeOfDay
twelveHour' = do
    h' <- read <$> (count 2 digit <|> count 1 digit)
    m  <- option 0 $ char ':' >> read <$> count 2 digit
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
            (read <$> (count 2 digit <|> count 1 digit)) <*>
            (char ':' >> time2) <*>
            (option 0 $ char ':' >> parsePico)
    nocolon = makeTimeOfDayValid <$>
              time2 <*>
              option 0 time2 <*>
              option 0 parsePico
    time2 = read <$> count 2 digit

timezone :: ReaderT Options Parser TimeZone
timezone = lift timezone'

timezone' :: Parser TimeZone
timezone' = (plus <|> minus) <*> timezone''
  where
    plus = char '+' >> return minutesToTimeZone
    minus = char '-' >> return (minutesToTimeZone . negate)
    two = read <$> count 2 digit
    one = read <$> count 1 digit
    hour p = (60*) <$> p
    minute p = option () (char ':' >> return ()) >> p
    timezone'' = choice [ (+) <$> hour two <*> minute two
                        , (+) <$> hour one <*> minute two
                        , hour two
                        , hour one
                        ]

defaultOptions :: Options
defaultOptions = Options [YMD,DMY,MDY] True Nothing Nothing (set ". /-") False

defaultParser :: ReaderT Options Parser Day
defaultParser = charSeparated <|>
                fourTwoTwo <|>
                twoTwoTwo <|>
                twoTwoFour

debugParse :: Options -> ReaderT Options Parser a ->
              B.ByteString -> Result a
debugParse opt p = flip feed B.empty . parse (runReaderT p opt)