module JsonParser
    ( parseJsonFile
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Pipes

import qualified Json
import Parse

whitespaceParser :: Parser ()
whitespaceParser = void $ A.takeWhile isWhitespace
    where
        isWhitespace c = case c of
            ' ' -> True
            '\n' -> True
            '\r' -> True
            '\t' -> True
            _ -> False

numberParser :: Parser Double
numberParser = A.double

jsonNumberParser :: StreamParser Json.Token
jsonNumberParser = do
    number <- numberParser
    return $ PartialResult (Just $ Json.Number number) Nothing False

stringParser :: Parser Text
stringParser = whitespaceParser *> A.char '"' *> stringContentParser <* whitespaceParser
    where
        stringContentParser :: Parser Text
        stringContentParser = do
            text <- A.takeTill (\c -> c == '"' || c == '\\')
            char <- A.anyChar
            case char of
                '"' -> return text
                '\\' -> do
                    escaped <- handleEscape
                    rest <- stringContentParser
                    return $ T.concat [text, T.singleton escaped, rest]
                _ -> error "Impossible!"

        handleEscape :: Parser Char
        handleEscape = do
            char <- A.anyChar
            case char of
                'b' -> return '\b'
                'f' -> return '\f'
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                'u' -> do
                    digits <- A.take 4
                    case A.parseOnly (A.hexadecimal <* A.endOfInput) digits of
                        Right num -> return $ chr num
                        Left _ -> fail "\\u escape sequence must always be followed by exactly 4 digits"
                _ -> return char

jsonStringParser :: StreamParser Json.Token
jsonStringParser = do
    string <- stringParser
    return $ PartialResult (Just $ Json.String string) Nothing False

booleanParser :: Parser Bool
booleanParser = (A.string "true" $> True) <|> (A.string "false" $> False)

jsonBooleanParser :: StreamParser Json.Token
jsonBooleanParser = do
    boolean <- booleanParser
    return $ PartialResult (Just $ Json.Boolean boolean) Nothing False

nullParser :: Parser ()
nullParser = void $ A.string "null"

jsonNullParser :: StreamParser Json.Token
jsonNullParser = nullParser >> return (PartialResult (Just Json.Null) Nothing False)

jsonArrayParser :: StreamParser Json.Token
jsonArrayParser = do
    char <- A.peekChar'
    case char of
        '[' -> return $ PartialResult (Just Json.ArrayBegin) (Just jsonArrayElementParser) False
        _ -> fail "not an array"
    where
        jsonArrayElementParser :: StreamParser Json.Token
        jsonArrayElementParser = do
            char <- A.anyChar <* whitespaceParser
            case char of
                '[' -> (A.char ']' $> PartialResult (Just Json.ArrayEnd) Nothing False)
                        <|> return (PartialResult Nothing (Just jsonTokenParser) True)
                ',' -> return $ PartialResult Nothing (Just jsonTokenParser) True
                ']' -> return $ PartialResult (Just Json.ArrayEnd) Nothing False
                _ -> fail $ "expected ']' or ',' but found '" ++ [char] ++ "'"

jsonObjectParser :: StreamParser Json.Token
jsonObjectParser = do
    char <- A.peekChar'
    case char of
        '{' -> return $ PartialResult (Just Json.ObjectBegin) (Just jsonObjectFieldParser) False
        _ -> fail "not an object"
    where
        jsonObjectFieldParser :: StreamParser Json.Token
        jsonObjectFieldParser = do
            char <- A.anyChar <* whitespaceParser
            case char of
                '{' -> (A.char '}' $> PartialResult (Just Json.ObjectEnd) Nothing False)
                    <|> return (PartialResult Nothing (Just jsonObjectKeyParser) True)
                ',' -> return $ PartialResult Nothing (Just jsonObjectKeyParser) True
                '}' -> return $ PartialResult (Just Json.ObjectEnd) Nothing False
                _ -> fail $ "expected '}' or ',' but found '" ++ [char] ++ "'"

        jsonObjectKeyParser :: StreamParser Json.Token
        jsonObjectKeyParser = do
            key <- stringParser <* A.char ':'
            return $ PartialResult (Just $ Json.ObjectField key) (Just jsonTokenParser) False

-- jsonObjectParser :: StreamParser Json.Token
-- jsonObjectParser = do
--     _ <- A.char '{'

--     where
--         jsonObjectEndParser :: StreamParser Json.Token
--         jsonObjectEndParser = do
--             char <- A.peekChar'
--             case char of
--                 '}' -> A.anyChar $> PartialResult (Just Json.ObjectEnd) Nothing False
--                 ',' -> A.anyChar $> PartialResult Nothing (Just jsonObjectFieldParser) True
--                 _ -> return $ PartialResult Nothing (Just jsonObjectFieldParser) True

--         jsonObjectFieldParser :: StreamParser Json.Token
--         jsonObjectFieldParser = do
--             key <- stringParser <* A.char ':'
--            char <- A.peekChar'

-- jsonObjectBeginParser :: StreamParser Json.Token
-- jsonObjectBeginParser = do
--     _ <- A.char '{'
--     parseContinue' Json.ObjectBegin jsonObjectFieldParser

-- jsonObjectFieldParser :: StreamParser Json.Token
-- jsonObjectFieldParser = do
--     key <- stringParser
--     _ <- A.char ':'
--     PartialResult value parser <- jsonTokenParser
--     case parser of
--         Just p -> case value of
--             Just v -> parseContinue' (Json.ObjectField (key, v)) p
--             Nothing -> parseContinue Nothing p
--         Nothing -> parseContinue value jsonObjectEndParser

-- jsonObjectEndParser :: StreamParser Json.Token
-- jsonObjectEndParser = do
--     char <- A.anyChar
--     case char of
--         '}' -> parseContinue' Json.ObjectEnd jsonTokenParser
--         ',' -> jsonObjectFieldParser
--         _ -> fail $ "expected ',' or '}' but found " ++ [char]

-- jsonWhitespaceParser :: StreamParser Json.Token
-- jsonWhitespaceParser = do
--     _ <- whitespaceParser
--     parseContinue Nothing jsonTokenParser

jsonTokenParser :: StreamParser Json.Token
jsonTokenParser = whitespaceParser *> (
        jsonBooleanParser <|>
        jsonNullParser <|>
        jsonObjectParser <|>
        jsonArrayParser <|>
        -- jsonArrayEndParser <|>
        jsonNumberParser <|>
        jsonStringParser
        -- jsonWhitespaceParser
    ) <* whitespaceParser

parseJsonFile :: FilePath -> Producer Json.Token IO ()
parseJsonFile filePath = parserInput (streamFile filePath) >-> parse jsonTokenParser
