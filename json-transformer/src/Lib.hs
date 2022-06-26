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
someFunc = runEffect $ parseJsonFile "testfile.json" >-> printCurrentPath

printCurrentPath :: Consumer Json.Token IO ()
printCurrentPath = flip evalStateT Json.rootPath $ forever $ do
    token <- lift await

    case token of
        Json.ObjectBegin -> return ()
        Json.ObjectField member -> modify $ flip Json.enterObject member
        Json.ArrayBegin -> modify $ flip Json.enterArray 0
        Json.ArrayEnd -> modify Json.exitPath
        _ -> do
            path <- get
            lift $ lift $ putStr $ show path ++ " = "
            modify Json.exitPath
            case Json.getLeaf path of
                Json.ArrayPath index _ -> modify $ flip Json.enterArray (index + 1)
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
