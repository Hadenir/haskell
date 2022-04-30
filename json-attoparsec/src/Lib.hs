module Lib
    ( someFunc
    ) where

import Data.Attoparsec.Text as P
import JsonParser

someFunc :: IO ()
someFunc = print (P.parseOnly stringParser "\"aasasd\"")
