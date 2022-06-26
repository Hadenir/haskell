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
import ParserCommon

whitespaceParser :: Parser ()
whitespaceParser = void $ A.takeWhile isWhitespace
    where
        isWhitespace c = case c of
            ' ' -> True
            '\n' -> True
            '\r' -> True
            '\t' -> True
            _ -> False

jsonNumberParser :: StreamParser Json.Token
jsonNumberParser = do
    number <- numberParser
    parseFinish $ Json.Number number

jsonStringParser :: StreamParser Json.Token
jsonStringParser = do
    string <- whitespaceParser *> stringParser <* whitespaceParser
    parseFinish $ Json.String string

jsonBooleanParser :: StreamParser Json.Token
jsonBooleanParser = do
    boolean <- booleanParser
    parseFinish $ Json.Boolean boolean

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
            char <- whitespaceParser *> A.anyChar <* whitespaceParser
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
            char <- whitespaceParser *> A.anyChar <* whitespaceParser
            case char of
                '{' -> (A.char '}' *> parseFinish Json.ObjectEnd) <|> parseNext jsonObjectKeyParser
                ',' -> parseNext jsonObjectKeyParser
                '}' -> parseFinish Json.ObjectEnd
                _ -> fail $ "expected '}' or ',' but found '" ++ [char] ++ "'"

        jsonObjectKeyParser :: StreamParser Json.Token
        jsonObjectKeyParser = do
            key <- whitespaceParser *> stringParser <* whitespaceParser <* A.char ':'
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
