module Data.Time.Parsers.Tables (months) where

import Data.Map
import Data.ByteString (ByteString)

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