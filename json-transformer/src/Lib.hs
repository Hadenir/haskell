module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import System.IO

import qualified Json
import JsonParser
import JsonTransformer

someFunc :: IO ()
someFunc = runEffect $ parseJsonFile "testfile.json" >-> transform >-> prettyPrint

prettyPrint :: Consumer Json.Token IO ()
prettyPrint = flip evalStateT 0 $ forever $ do
    token <- lift await
    case token of
        Json.ArrayEnd -> modify $ subtract 1
        Json.ObjectEnd -> modify $ subtract 1
        _ -> return ()
    depth <- get
    lift $ lift $ do
        putStr $ concat $ replicate depth "  "
        case token of
            Json.Null -> putStrLn "null"
            Json.Boolean bool ->  print bool
            Json.String text -> T.putStrLn $ T.concat ["\"", text, "\""]
            Json.Number num -> print num
            Json.ArrayBegin -> putStrLn "["
            Json.ArrayEnd -> putStrLn "]"
            Json.ObjectBegin -> putStrLn "{"
            Json.ObjectEnd -> putStrLn "}"
            Json.ObjectField key -> T.putStrLn $ T.concat ["\"", key, "\":"]
    case token of
        Json.ArrayBegin -> modify (+1)
        Json.ObjectBegin -> modify (+1)
        _ -> return ()
