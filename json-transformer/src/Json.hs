-- | Module containing data structure for representing JSON objects.
module Json
    ( Token (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Data type for representing single JSON tokens. Contains following variants:
--
--   * `Null` - `null` value
--   * `Boolean` - `true`/`false` value
--   * `String` - string of characters value
--   * `Number` - real number value
--   * `ArrayBegin` - beggining of an array, `[`
--   * `ArrayEnd` - end of an array, `]`
--   * `ObjectBegin` - beggining of an object, `{`
--   * `ObjectField` - object field with given name
--   * `ObjectEnd` - end of an object, `}`
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
