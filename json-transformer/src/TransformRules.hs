-- | This module contains data types for supported transformations.
module TransformRules
    ( TransformRule (..)
    , TransformRules
    , findTransform
    , removeTransform
    ) where


import Data.Map.Strict as M
import Data.Text
import Json
import JsonPath

-- | Data type representing single transformation. It has following variants:
--
--   * `SetValueNull` - setting given field to null
--   * `SetValueBool` - setting given field to constant boolean
--   * `SetValueString` - setting given field to constant string
--   * `SetValueNumber` - setting given field to constant number
--   * `SetValueNull` - removing given field
data TransformRule = SetValueNull JsonPath
    | SetValueBool Bool JsonPath
    | SetValueString Text JsonPath
    | SetValueNumber Double JsonPath
    | RemoveEntry JsonPath
    deriving (Show, Eq)

-- | Helper class for handling set of `TransformRule`s.
type TransformRules = [TransformRule]

getPath :: TransformRule -> JsonPath
getPath (SetValueNull path) = path
getPath (SetValueBool _  path) = path
getPath (SetValueString _ path) = path
getPath (SetValueNumber _ path) = path
getPath (RemoveEntry path) = path

-- | Searches through set of rules and returns one that matches provided json path.
--   If there is no applicable transformation, returns `Nothing`.
findTransform :: TransformRules -> JsonPath -> Maybe TransformRule
findTransform [] _ = Nothing
findTransform (rule:rs) path
    | getPath rule <= path = Just rule
    | otherwise = findTransform rs path

-- | Removes given transformation from the set.
--   Current path must be provided, because some rules are applicable to multiple
--   field in JSON and such rules should not be removed.
removeTransform :: TransformRules -> Maybe TransformRule -> JsonPath -> TransformRules
removeTransform rules Nothing _ = rules
removeTransform [] _ _ = []
removeTransform (r:rs) m@(Just rule) path
    | r == rule && getPath r == path = rs
    | otherwise = r : removeTransform rs m path
