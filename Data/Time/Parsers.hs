{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Parsers where

import Data.Time.Parsers.Types
import Data.Time.Parsers.Util

import Control.Applicative                  ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8                as A
import Data.Attoparsec.FastSet
import Data.Time
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
    m <- asks makeRecent
    lift $ yearDayOfYear' s m

yearDayOfYear' :: FastSet -> Bool -> Parser Day
yearDayOfYear' seps' makeRecent' = do
    year <- (parseDateToken seps' >>= getYear)
    lastDay <- return $ if isLeapYear' year then 366 else 365
    _ <- separator
    days <- (decimal >>= \x -> if x <= lastDay then return x else mzero)
    return $ addDays days year
  where
    separator = satisfy $ flip memberChar seps'
    getYear (Year y) = return $ fromGregorian y 0 0
    getYear (Any y)  = return . (if makeRecent' then forceRecent else id) $
                       fromGregorian y 0 0
    getYear _        = fail "Invalid Year"
    isLeapYear' day  = (\(y,_,_) -> isLeapYear y) $ toGregorian day

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

defaultOptions :: Options
defaultOptions = Options [YMD,DMY,MDY] True Nothing Nothing (set ". /-")

defaultParser :: ReaderT Options Parser Day
defaultParser = charSeparated <|>
                fourTwoTwo <|>
                twoTwoTwo <|>
                twoTwoFour

parseDate :: Options -> ReaderT Options Parser Day ->
             B.ByteString -> Result Day
parseDate opt p = flip feed B.empty . parse (runReaderT p opt)