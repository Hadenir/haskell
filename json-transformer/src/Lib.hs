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

-- | Opens file at given path and executes transformation rules parser on its contents.
--   Returns parser result on success.
readRulesFile :: FilePath -> IO TransformRules
readRulesFile filePath = do
    content <- T.readFile filePath
    case parseTransformRules content of
        Right rules -> return rules
        Left errorMsg -> error $ "Failed to parse transform rules: " ++ errorMsg

-- | Given input file path, output file path and a set of transform rules,
--   transforms contents of the input file and saves results in the output file.
transformFile :: FilePath -> FilePath -> TransformRules -> IO ()
transformFile inputPath outputPath rules =
    runEffect $ parseJsonFile inputPath >-> includePaths >-> transform rules >-> saveJsonFile outputPath

-- | Stream consumer that awaits JSON tokens and saves them properely formatted to a file.
saveJsonFile :: FilePath -> Consumer Json.Token IO ()
saveJsonFile filePath = flip evalStateT (0, False, False) $ do
    file <- liftIO $ openFile filePath WriteMode
    forever $ do
        token <- lift await
        case token of
            Json.ArrayEnd -> modify $ \(d,_,_) -> (d-1,False,True)
            Json.ObjectEnd -> modify $ \(d,_,_) -> (d-1,False,True)
            _ -> return ()
        (depth, comma, newline) <- get
        liftIO $ do
            hPutStr file $ if comma then  "," else ""
            hPutStr file $ if newline then "\n" ++ concat (replicate depth "  ") else " "
            case token of
                Json.Null -> hPutStr file "null"
                Json.Boolean bool ->  hPutStr file $ if bool then "true" else "false"
                Json.String text -> T.hPutStr file $ T.concat ["\"", text, "\""]
                Json.Number num -> hPutStr file $ show num
                Json.ArrayBegin -> hPutStr file "["
                Json.ArrayEnd -> hPutStr file "]"
                Json.ObjectBegin -> hPutStr file "{"
                Json.ObjectEnd -> hPutStr file "}"
                Json.ObjectField key -> T.hPutStr file $ T.concat ["\"", key, "\":"]
        case token of
            Json.ArrayBegin -> modify $ \(d,_,_) -> (d+1,False,True)
            Json.ObjectBegin -> modify $ \(d,_,_) -> (d+1,False,True)
            Json.ObjectField _ -> modify $ \(d,_,_) -> (d,False,False)
            _ -> modify $ \(d,_,_) -> (d,True,True)
        liftIO $ hFlush file

-- | Stream consumer that awaits JSON tokens and prints them properely formatted to stdout.
prettyPrint :: Consumer Json.Token IO ()
prettyPrint = flip evalStateT (0, False, False) $ forever $ do
    token <- lift await
    case token of
        Json.ArrayEnd -> modify $ \(d,_,_) -> (d-1,False,True)
        Json.ObjectEnd -> modify $ \(d,_,_) -> (d-1,False,True)
        _ -> return ()
    (depth, comma, newline) <- get
    liftIO $ do
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
