{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections#-}

module Data.Time.Parsers.Tables ( weekdays
                                , months
                                , timeZones
                                , ausTimeZones
                                ) where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.Map              hiding (map)
import Data.Time             (TimeZone(..))

weekdays :: [ByteString]
weekdays = [ "Monday", "Mon"
           , "Tuesday", "Tue", "Tues"
           , "Wednesday", "Wed", "Weds"
           , "Thursday", "Thu", "Thur", "Thurs"
           , "Friday", "Fri"
           , "Saturday", "Sat"
           , "Sunday", "Sun"
           ]

months :: Map ByteString Integer
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

mkTimeZone :: (ByteString, Int) -> (ByteString, TimeZone)
mkTimeZone (name, minutes) = (name,) . TimeZone minutes False $ unpack name

timeZones :: Map ByteString TimeZone
timeZones = fromList . map mkTimeZone $
            [ ("NZDT",780), ("IDLE",720), ("NZST",720), ("NZT",720)
            , ("AESST",660), ("ACSST",630), ("CADT",630), ("SADT",630)
            , ("AEST",600), ("EAST",600), ("GST",600), ("LIGT",600)
            , ("SAST",570), ("CAST",570), ("AWSST",540), ("JST",540)
            , ("KST",540), ("MHT",540), ("WDT",540), ("MT",510)
            , ("AWST",480), ("CCT",480), ("WADT",480), ("WST",480)
            , ("JT",450), ("ALMST",420), ("WAST",420), ("CXT",420)
            , ("MMT",390), ("ALMT",360), ("MAWT",360), ("IOT",300)
            , ("MVT",300), ("TFT",300), ("AFT",270), ("EAST",240)
            , ("MUT",240), ("RET",240), ("SCT",240), ("IRT",180)
            , ("IT",210), ("EAT",180), ("BT",180), ("EETDST",180)
            , ("HMT",180), ("BDST",120), ("CEST",120), ("CETDST",120)
            , ("EET",120), ("FWT",120), ("IST",120), ("MEST",120)
            , ("METDST",120), ("SST",120), ("BST",60), ("CET",60)
            , ("DNT",60), ("FST",60), ("MET",60), ("MEWT",60)
            , ("MEZ",60), ("NOR",60), ("SET",60), ("SWT",60)
            , ("WETDST",60), ("GMT",0), ("UT",0), ("UTC",0)
            , ("Z",0), ("ZULU",0), ("WET",0), ("WAT",-60)
            , ("FNST",-60), ("FNT",-120), ("BRST",-120), ("NDT",-150)
            , ("ADT",-180), ("AWT",-180), ("BRT",-180), ("NFT",-210)
            , ("NST",-210), ("AST",-240), ("ACST",-240), ("EDT",-240)
            , ("ACT",-300), ("CDT",-300), ("EST",-300), ("CST",-360)
            , ("MDT",-360), ("MST",-420), ("PDT",-420), ("AKDT",-480)
            , ("PST",-480), ("YDT",-480), ("AKST",-540), ("HDT",-540)
            , ("YST",-540), ("MART",-570), ("AHST",-600), ("HST",-600)
            , ("CAT",-600), ("NT",-660), ("IDLW",-720)
            ]

ausTimeZones :: Map ByteString TimeZone
ausTimeZones = fromList . map mkTimeZone $
               [("ACST",570), ("CST", 630), ("EST",600), ("SAT",570)]