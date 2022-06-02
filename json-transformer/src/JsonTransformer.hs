module JsonTransformer
    ( transform
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Pipes

import qualified Json

-- | Transforms JSON keeping only object fields named "first_name".
transform :: Monad m => Pipe Json.Token Json.Token m ()
transform = keepOnly "first_name" >-> discardEmptyFields

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
