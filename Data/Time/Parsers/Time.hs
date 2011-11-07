{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Parsers.Time ( twelveHour
                              , twentyFourHour
                              , defaultTimeOfDay
                              ) where

import Data.Time.Parsers.Util

import Control.Applicative             ((<$>),(<*>),(<|>))
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.Char8
import Data.Char                       (toUpper)
import Data.Fixed                      (Pico)
import Data.Time
import Prelude                         hiding (takeWhile)

--Time Parsers

parsePico :: Parser Pico
parsePico = (+) <$> (fromInteger <$> decimal) <*> (option 0 postradix)
  where
    postradix = do
        _ <- char '.'
        bs <- takeWhile isDigit
        let i = fromInteger . read . B.unpack $ bs
            l = B.length bs
        return (i/10^l)

twelveHour :: OptionedParser TimeOfDay
twelveHour = lift twelveHour'

twelveHour' :: Parser TimeOfDay
twelveHour' = do
    h'   <- (nDigit 2 <|> nDigit 1)
    m    <- option 0 $ char ':' >> nDigit 2
    s    <- option 0 $ char ':' >> parsePico
    ampm <- B.map toUpper <$> (skipSpace >> (stringCI "AM" <|> stringCI "PM"))
    h    <- case ampm of
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

twentyFourHour :: OptionedParser TimeOfDay
twentyFourHour = lift twentyFourHour'

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

defaultTimeOfDay :: OptionedParser TimeOfDay
defaultTimeOfDay = twelveHour <|> twentyFourHour