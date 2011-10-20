{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Data.Time.Parsers where

import Data.Time.Parsers.Types

import Control.Applicative                  ((<$>))
import Control.Monad                        (MonadPlus, msum)
import Data.Attoparsec.Char8
import Data.Char                            (toLower)
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map                             as M

months :: Map B.ByteString Int
months = fromList [ ("january", 1),   ("jan", 1)
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

namedMonth :: B.ByteString -> Maybe Int
namedMonth = flip M.lookup months . B.map toLower

makeDates :: forall (m :: * -> *). Monad m =>
             B.ByteString -> B.ByteString -> B.ByteString -> Dates m
makeDates x y z = Dates { ymd = makeDate x y z
                        , mdy = makeDate z x y
                        , dmy = makeDate z y x
                        }

makeDate :: forall (m :: * -> *). Monad m =>
            B.ByteString -> B.ByteString -> B.ByteString -> m Day
makeDate ys ms ds = case (p ys, p ms, p ds) of
    (True, True, True)  -> makeDate' y m d
    (True, False, True) -> maybe (fail "Invalid Month Name")
                                 return
                                 (namedMonth ms) >>=
                           (\m' -> makeDate' y m' d)
    _                   -> fail "Non-numeric Values"
  where
    (y,m,d) = ( read $ B.unpack ys
              , read $ B.unpack ms
              , read $ B.unpack ds)
    p       = B.all isDigit

makeDate' :: forall (m :: * -> *). Monad m =>
             Integer -> Int -> Int -> m Day
makeDate' y m d = if validDate
                  then return $ fromGregorian y m d
                  else fail "Invalid date range"
  where
    validDate = and [ m > 0
                    , d > 0
                    , m <= 12
                    , d <= (gregorianMonthLength y m)
                    ]

forceRecent :: Day -> Day
forceRecent day | y < 100 && y <= 70 = addGregorianYearsClip 2000 day
                | y < 100            = addGregorianYearsClip 1900 day
                | otherwise          = day
  where
    (y,_,_) = toGregorian day

tryPolicies :: forall (m :: * -> *) a. MonadPlus m =>
               [Dates m -> m a] -> Dates m -> m a
tryPolicies formats dates = msum $ Prelude.map ( $ dates) formats

count2 :: Parser B.ByteString
count2 = B.pack <$> count 2 digit

count4 :: Parser B.ByteString
count4 = B.pack <$> count 4 digit
