module JsonTransformer
    ( transform
    , includePath
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Pipes

import qualified Json
import JsonPath
import TransformRules

import Debug.Trace

-- | Stream transformer that for each Json Token yields pair of two values:
--   * Path of the token;
--   * The token itself.
includePath :: Monad m => Pipe Json.Token (JsonPath, Json.Token) m ()
includePath = flip evalStateT rootPath $ forever $ do
    token <- lift await
    currentPath <- get
    lift $ yield (currentPath, token)

     -- Update current path
    case token of
        Json.ObjectBegin -> return ()
        Json.ObjectField member -> modify $ flip enterObject member
        Json.ArrayBegin -> modify $ flip enterArray 0
        Json.ArrayEnd -> modify exitPath
        _ -> do
            modify exitPath
            case getLeaf currentPath of
                ArrayPath index _ -> modify $ flip enterArray (index + 1)
                _ -> return ()

transform :: Monad m => TransformRules -> Pipe (JsonPath, Json.Token) Json.Token m ()
transform rules = forever $ do
    (path, token) <- await

    case findTransform rules path of
        Just trans -> exec trans token
        Nothing -> yield token

    where
        exec :: Monad m => TransformRule -> Json.Token -> Pipe (JsonPath, Json.Token) Json.Token m ()
        exec _ t@Json.ObjectBegin = yield t
        exec _ t@(Json.ObjectField _) = yield t
        exec _ t@Json.ObjectEnd = yield t
        exec _ t@Json.ArrayBegin = yield t
        exec _ t@Json.ArrayEnd = yield t
        exec (SetValueNull _) _ = yield Json.Null
        exec (SetValueBool bool _) _ = yield $ Json.Boolean bool
        exec (SetValueString text _) _ = yield $ Json.String text
        exec (SetValueNumber num _) _ = yield $ Json.Number num

-- | Stream transformer of JSON tokens that discards every value
--   that isn't in an object field of passed name.
keepOnly :: Monad m => Text -> Pipe Json.Token Json.Token m ()
keepOnly fieldName = flip evalStateT [] $ forever $ do
    token <- lift await
    handle token
    where
        handle token@(Json.ObjectField key) = modify (key:) >> lift (yield token)
        handle token = do
            keys <- get
            case keys of
                [] -> lift $ yield token
                (key:ks) -> case token of
                    Json.ObjectBegin -> lift $ yield token
                    Json.ObjectEnd -> lift $ yield token
                    Json.ArrayBegin -> lift $ yield token
                    Json.ArrayEnd -> lift $ yield token
                    _ -> do
                        put ks
                        when (key == fieldName) $ lift $ yield token

-- | Stream transformer of JSON tokens that discards empty object fields
--   which may occur when discarding their values.
discardEmptyFields :: Monad m => Pipe Json.Token Json.Token m ()
discardEmptyFields = flip evalStateT Nothing $ forever $ do
    token <- lift await
    handle token
    where
        handle token@(Json.ObjectField _) = put $ Just token
        handle token@Json.ArrayEnd = put Nothing >> lift (yield token)
        handle token@Json.ObjectEnd = put Nothing >> lift (yield token)
        handle token = do
            field <- get
            forM_ field (lift . yield)
            put Nothing
            lift $ yield token
