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
    parseFinish $ Json.Number number

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
    parseFinish $ Json.String string

booleanParser :: Parser Bool
booleanParser = (A.string "true" $> True) <|> (A.string "false" $> False)

jsonBooleanParser :: StreamParser Json.Token
jsonBooleanParser = do
    boolean <- booleanParser
    parseFinish $ Json.Boolean boolean

nullParser :: Parser ()
nullParser = void $ A.string "null"

jsonNullParser :: StreamParser Json.Token
jsonNullParser = nullParser >> parseFinish Json.Null

jsonArrayParser :: StreamParser Json.Token
jsonArrayParser = do
    char <- A.peekChar'
    case char of
        '[' -> parseContinue Json.ArrayBegin jsonArrayElementParser
        _ -> fail "not an array"
    where
        jsonArrayElementParser :: StreamParser Json.Token
        jsonArrayElementParser = do
            char <- A.anyChar <* whitespaceParser
            case char of
                '[' -> (A.char ']' *> parseFinish Json.ArrayEnd) <|> parseNext jsonTokenParser
                ',' -> parseNext jsonTokenParser
                ']' -> parseFinish Json.ArrayEnd
                _ -> fail $ "expected ']' or ',' but found '" ++ [char] ++ "'"

jsonObjectParser :: StreamParser Json.Token
jsonObjectParser = do
    char <- A.peekChar'
    case char of
        '{' -> parseContinue Json.ObjectBegin jsonObjectFieldParser
        _ -> fail "not an object"
    where
        jsonObjectFieldParser :: StreamParser Json.Token
        jsonObjectFieldParser = do
            char <- A.anyChar <* whitespaceParser
            case char of
                '{' -> (A.char '}' *> parseFinish Json.ObjectEnd) <|> parseNext jsonObjectKeyParser
                ',' -> parseNext jsonObjectKeyParser
                '}' -> parseFinish Json.ObjectEnd
                _ -> fail $ "expected '}' or ',' but found '" ++ [char] ++ "'"

        jsonObjectKeyParser :: StreamParser Json.Token
        jsonObjectKeyParser = do
            key <- stringParser <* A.char ':'
            parseContinue (Json.ObjectField key) jsonTokenParser

jsonTokenParser :: StreamParser Json.Token
jsonTokenParser = whitespaceParser *> (
        jsonBooleanParser <|>
        jsonNullParser <|>
        jsonObjectParser <|>
        jsonArrayParser <|>
        jsonNumberParser <|>
        jsonStringParser
    ) <* whitespaceParser

parseJsonFile :: FilePath -> Producer Json.Token IO ()
parseJsonFile filePath = parserInput (streamFile filePath) >-> parse jsonTokenParser
