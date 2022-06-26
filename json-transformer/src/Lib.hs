module Lib
    ( readRulesFile
    , transformFile
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import System.IO

import qualified Json
import JsonPath
import JsonParser
import JsonTransformer
import TransformRules
import TransformRulesParser

readRulesFile :: FilePath -> IO TransformRules
readRulesFile filePath = do
    content <- T.readFile filePath
    case parseTransformRules content of
        Right rules -> return rules
        Left errorMsg -> error $ "Failed to parse transform rules: " ++ errorMsg

transformFile :: FilePath -> TransformRules -> IO ()
transformFile filePath rules = runEffect $ parseJsonFile filePath >-> includePaths >-> transform rules >-> prettyPrint

prettyPrint :: Consumer Json.Token IO ()
prettyPrint = flip evalStateT (0, False, False) $ forever $ do
    token <- lift await
    case token of
        Json.ArrayEnd -> modify $ \(d,_,_) -> (d-1,False,True)
        Json.ObjectEnd -> modify $ \(d,_,_) -> (d-1,False,True)
        _ -> return ()
    (depth, comma, newline) <- get
    lift $ lift $ do
        putStr $ if comma then  "," else ""
        putStr $ if newline then "\n" ++ concat (replicate depth "  ") else " "
        case token of
            Json.Null -> putStr "null"
            Json.Boolean bool ->  putStr $ if bool then "true" else "false"
            Json.String text -> T.putStr $ T.concat ["\"", text, "\""]
            Json.Number num -> putStr $ show num
            Json.ArrayBegin -> putStr "["
            Json.ArrayEnd -> putStr "]"
            Json.ObjectBegin -> putStr "{"
            Json.ObjectEnd -> putStr "}"
            Json.ObjectField key -> T.putStr $ T.concat ["\"", key, "\":"]
    case token of
        Json.ArrayBegin -> modify $ \(d,_,_) -> (d+1,False,True)
        Json.ObjectBegin -> modify $ \(d,_,_) -> (d+1,False,True)
        Json.ObjectField _ -> modify $ \(d,_,_) -> (d,False,False)
        _ -> modify $ \(d,_,_) -> (d,True,True)
