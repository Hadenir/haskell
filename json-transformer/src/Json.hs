-- | Module containing data structure for representing JSON objects.
module Json
    ( Token (..)
    ) where

import Data.Text (Text)

data Token = Null
    | Boolean Bool
    | String Text
    | Number Double
    | ArrayBegin
    | ArrayEnd
    | ObjectBegin
    | ObjectField Text
    | ObjectEnd
    deriving Show
