{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Data.Time.Parsers where

import Data.Time.Parsers.Types

import Control.Applicative                  ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import Data.Attoparsec.Char8                as A
import Data.Char                            (toLower)
import Data.Time
import qualified Data.ByteString.Char8      as B
import qualified Data.Map                   as M

months :: M.Map B.ByteString Integer
months = M.fromList [ ("january", 1),   ("jan", 1)
                    , ("february", 2),  ("feb", 2)
                    , ("march", 3),     ("mar", 3)
                    , ("april", 4),     ("apr", 4)
                    , ("may", 5)
                    , ("june", 6),      ("jun", 6)
                    , ("july", 7),      ("jul", 7)
                    , ("august", 8),    ("aug", 8)
                    , ("september", 9), ("sep", 9), ("sept", 9)
                    , ("october", 10),  ("oct", 10)
                    , ("november", 11), ("nov", 11)
                    , ("december", 12), ("dec", 12)
                    ]

namedMonth :: B.ByteString -> Maybe Integer
namedMonth = flip M.lookup months . B.map toLower

readDateToken :: forall (m :: * -> *). Monad m => B.ByteString -> m DateToken
readDateToken bs |notDigits bs     = maybe (fail $ "Invalid DateToken" ++
                                                   (B.unpack bs))
                                           (return . Month)
                                           (namedMonth bs)
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
forceRecent day | y < 100 && y <= 70 = addGregorianYearsClip 2000 day
                | y < 100            = addGregorianYearsClip 1900 day
                | otherwise          = day
  where
    (y,_,_) = toGregorian day

tryFormats :: forall (m :: * -> *). MonadPlus m =>
              [DateFormat] -> (DateFormat -> m Day) -> m Day
tryFormats fs d = (msum $ Prelude.map d fs)

count2 :: Parser DateToken
count2 = Any . read <$> count 2 digit

count4 :: Parser DateToken
count4 = Year . read <$> count 4 digit

parseDateToken :: String -> Parser DateToken
parseDateToken seps' = readDateToken =<< (takeTill $ inClass seps')

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

charSeparated' :: String -> [DateFormat] -> Bool -> Parser Day
charSeparated' seps' formats' makeRecent' = do
    a   <- parseDateToken seps'
    sep <- satisfy $ inClass seps'
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

yearDayOfYear' :: String -> Bool -> Parser Day
yearDayOfYear' seps' makeRecent' = do
    year <- (parseDateToken seps' >>= getYear)
    lastDay <- return $ if isLeapYear' year then 366 else 365
    _ <- separator
    days <- (decimal >>= \x -> if x <= lastDay then return x else mzero)
    return $ addDays days year
  where
    separator = satisfy $ inClass seps'
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
             namedMonth =<< (A.takeWhile $ inClass "a-zA-Z")
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
defaultOptions = Options [YMD,DMY,MDY] True Nothing Nothing ". /-"

defaultParser :: ReaderT Options Parser Day
defaultParser = charSeparated <|>
                fourTwoTwo <|>
                twoTwoTwo <|>
                twoTwoFour

parseDate :: Options -> ReaderT Options Parser Day ->
             B.ByteString -> Result Day
parseDate opt p = flip feed B.empty . parse (runReaderT p opt)