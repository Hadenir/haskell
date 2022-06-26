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
import JsonPath
import JsonParser
import JsonTransformer
import TransformRules

rules :: TransformRules
rules = [
    SetValueString "Konrad" (RootPath $ ArrayPath 0 $ ObjectPath "first_name" NilPath),
    SetValueNull (RootPath $ ArrayPath 2 $ ObjectPath "ip_address" NilPath),
    SetValueBool True (RootPath $ ArrayPath 4 $ ObjectPath "a" $ ObjectPath "b" NilPath)
    ]

someFunc :: IO ()
someFunc = runEffect $ parseJsonFile "testfile.json" >-> includePath >-> transform rules >-> prettyPrint

printCurrentPath :: Consumer Json.Token IO ()
printCurrentPath = flip evalStateT rootPath $ forever $ do
    token <- lift await

    case token of
        Json.ObjectBegin -> return ()
        Json.ObjectField member -> modify $ flip enterObject member
        Json.ArrayBegin -> modify $ flip enterArray 0
        Json.ArrayEnd -> modify exitPath
        _ -> do
            path <- get
            lift $ lift $ putStr $ show path ++ " = "
            modify exitPath
            case getLeaf path of
                ArrayPath index _ -> modify $ flip enterArray (index + 1)
                _ -> return ()

    lift $ lift $ case token of
        Json.Null -> putStrLn "null"
        Json.Boolean b -> print b
        Json.String text -> putStrLn $ T.unpack text
        Json.Number number -> print number
        Json.ObjectEnd -> putStrLn "{}"
        _ -> return ()

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
            Json.Boolean bool ->  putStr $ show bool
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
