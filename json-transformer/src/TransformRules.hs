module TransformRules
    ( TransformRule (..)
    , TransformRules
    ) where

import Data.Text
import qualified Json

data TransformRule = SetValueNull Json.Path
    | SetValueBool Bool Json.Path
    | SetValueString Text Json.Path
    | SetValueNumber Double Json.Path

type TransformRules = [TransformRule]
