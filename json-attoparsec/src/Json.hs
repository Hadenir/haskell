module Json where

import qualified Data.Map.Strict as M
import Data.Text

data Value = Null
    | Boolean Bool
    | String Text
    | Number Double
    | Array [Value]
    | Object (M.Map Text Value)
    deriving (Show)
