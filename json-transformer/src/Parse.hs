-- | This module exports functionality that allows Attoparsec parsers
-- to be supplied with values from @Producer@s, and return values to @Consumer@s.
-- Both @Producer@s and @Consumer@s can also be @Pipe@s.
--
-- __Example:__
-- @
-- someFunc :: IO ()
-- someFunc =  runEffect $ for (parserInput $ streamFile "testfile.json") (lift . print)
-- @
module Parse
    ( StreamParser
    , ParsePartialResult (PartialResult)
    -- , parseFinish
    -- , parseFinish'
    -- , parseContinue
    -- , parseContinue'
    , parse
    , parserInput
    , streamFile
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Data (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import qualified Pipes.Prelude as P
import System.IO

-- | This type contains information about errors and causes of parsing failures.
data ParseException = ParseException {errorMsg :: String, context :: [String]}
    deriving (Show, Typeable)
instance Exception ParseException

-- | Each parser that is supposed to interoperate with pipes must return this type.
-- * First parameter is the parser's result. When a @Just@ value is supplied
--   it is yielded to the @Consumer@. For @Nothing@ no value is yielded.
-- * Second parameter represents parser to be used for further parsing.
--   Providing @Nothing@ means the parsing ends and the output terminates.
-- * Third parameter signals whether or not to keep current parser on the stack.
--   When set to @True@, parsing will come back to the parser returning this result,
--   after parser from second parameters finishes (or immediately if it's @Nothing@).
data ParsePartialResult a = PartialResult (Maybe a) (Maybe (StreamParser a)) Bool

-- | Helper type for conciseness.
type StreamParser a = Parser (ParsePartialResult a)

-- -- | Helper function that creates result indicating current parser is finished.
-- parseFinish :: Maybe a -> StreamParser a
-- parseFinish val = return $ PartialResult val Nothing

-- parseFinish' :: a -> StreamParser a
-- parseFinish' val = return $ PartialResult (Just val) Nothing

-- -- | Helper function that creates result indicating the parser can continue parsing.
-- parseContinue :: Maybe a -> StreamParser a -> StreamParser a
-- parseContinue val parser = return $ PartialResult val (Just parser)

-- parseContinue' :: a -> StreamParser a -> StreamParser a
-- parseContinue' val parser = return $ PartialResult (Just val) (Just parser)

-- | This function takes an Attoparsec parser returning @ParsePartialResult a@ and
--   transforms it into a Pipe that consumes @Maybe Text@s and yields parsed values.
--
--   Because parsers can be run incrementally, the function needs to know when the input ends.
--   Supplying the pipe with @Nothing@ is considered to indictate that.
parse :: Monad m => Parser (ParsePartialResult a) -> Pipe (Maybe Text) a m ()
parse parser = flip evalStateT [] $ do
    put [parser]
    chunk <- lift await
    forM_ chunk (go . A.parse parser)
    where
        go :: Monad m => A.Result (ParsePartialResult a) -> StateT [StreamParser a] (Pipe (Maybe Text) a m) ()
        go result = case result of
            A.Fail _ ctxt err -> throw $ ParseException err ctxt

            A.Partial cont -> do
                chunk <- lift await
                case chunk of
                    Nothing -> go $ cont T.empty
                    Just chunk -> go $ cont chunk

            -- A.Done rest (PartialResult result next) -> do
            --     forM_ result yield
            --     forM_ next (\parser -> go (A.parse parser rest))

            A.Done rest (PartialResult result next keep) -> do
                forM_ result (lift . yield)
                unless keep $ modify $ \(_:ps) -> ps
                forM_ next $ \next' -> modify (next':)
                parsers <- get
                case parsers of
                    (parser:_) -> go (A.parse (head parsers) rest)
                    [] -> return ()

-- | Helper function that takes a producer of @Text@s, wraps its
--   values into @Maybe@ and adds @Nothing@ at the end.
parserInput :: Monad m => Producer Text m r -> Producer (Maybe Text) m r
parserInput producer = for producer (yield . Just) <* yield Nothing

-- | Helper function that opens a file and reads its contents
--   yielding them as @Text@ chunks.
streamFile :: FilePath -> Producer Text IO ()
streamFile path = do
    handle <- lift $ openFile path ReadMode
    loop handle
    lift $ hClose handle
    where
        loop handle = do
            chunk <- lift $ T.hGetChunk handle
            unless (T.null chunk) $ do
                yield chunk
                loop handle
