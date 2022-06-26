module TransformRules
    ( TransformRule (..)
    , TransformRules
    , findTransform
    ) where

import Data.Text
import JsonPath

data TransformRule = SetValueNull JsonPath
    | SetValueBool Bool JsonPath
    | SetValueString Text JsonPath
    | SetValueNumber Double JsonPath
    deriving Show

type TransformRules = [TransformRule]

getPath :: TransformRule -> JsonPath
getPath (SetValueNull path) = path
getPath (SetValueBool _  path) = path
getPath (SetValueString _ path) = path
getPath (SetValueNumber _ path) = path

findTransform :: TransformRules -> JsonPath -> Maybe TransformRule
findTransform [] _ = Nothing
findTransform (rule:rs) path
    | getPath rule == path = Just rule
    | otherwise = findTransform rs path
