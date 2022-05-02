module Json
    ( Value (..)
    ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)

data Value = Null
    | Boolean Bool
    | String Text
    | Number Double
    | Array [Value]
    | Object (M.Map Text Value)

instance Show Value where
    show = showJson 0

prefix :: Int -> String
prefix depth = replicate (depth * 4) ' '

showJson :: Int -> Value -> String
showJson _ Null = "null"
showJson _ (Boolean b) = show b
showJson _ (String text) = show text
showJson _ (Number num) = show num
showJson _ (Array []) = "[]"
showJson depth (Array arr) = "[\n" ++ showJsonArray arr ++ prefix depth ++ "]"
    where
        depth1 = depth + 1
        showJsonArray [] = ""
        showJsonArray [v] = prefix depth1 ++ showJson depth1 v ++ "\n"
        showJsonArray (v:vs) = prefix depth1 ++ showJson depth1 v ++ ",\n" ++ showJsonArray vs
showJson depth (Object members)
    | M.null members = "{}"
    | otherwise = "{\n" ++ showJsonMembers (M.assocs members) ++ prefix depth  ++ "}"
    where
        depth1 = depth + 1
        showJsonMembers [] = ""
        showJsonMembers [(k,v)] = prefix depth1 ++ show k ++ ": " ++ showJson depth1 v ++ "\n"
        showJsonMembers ((k,v):ps) = prefix depth1 ++ show k ++ ": " ++ showJson depth1 v ++ ",\n" ++ showJsonMembers ps
