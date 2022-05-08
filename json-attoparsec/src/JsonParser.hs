{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module JsonParser
    ( jsonParser
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text.Lazy as P
import Data.Char
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import qualified Json

whitespaceParser :: Parser ()
whitespaceParser = void $ P.takeWhile isWhitespace
    where
        isWhitespace c = case c of
            ' ' -> True
            '\n' -> True
            '\r' -> True
            '\t' -> True
            _ -> False

numberParser :: Parser Double
numberParser = P.double

jsonNumberParser :: Parser Json.Value
jsonNumberParser = Json.Number <$> numberParser

stringParser :: Parser Text
stringParser = P.char '"' *> stringContentParser
    where
        stringContentParser :: Parser Text
        stringContentParser = do
            text <- P.takeTill (\c -> c == '"' || c == '\\')
            char <- P.anyChar
            case char of
                '"' -> return text
                '\\' -> do
                    escaped <- handleEscape
                    rest <- stringContentParser
                    return $ T.concat [text, T.singleton escaped, rest]
                _ -> error "Impossible!"

        handleEscape :: Parser Char
        handleEscape = do
            char <- P.anyChar
            case char of
                'b' -> return '\b'
                'f' -> return '\f'
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                'u' -> do
                    digits <- P.take 4
                    case P.parseOnly (P.hexadecimal <* P.endOfInput) (fromStrict digits) of
                        Right num -> return $ chr num
                        Left _ -> fail "\\u escape sequence must always be followed by exactly 4 digits"
                _ -> return char

jsonStringParser :: Parser Json.Value
jsonStringParser = Json.String <$> stringParser

booleanParser :: Parser Bool
booleanParser = (P.string "true" $> True) <|> (P.string "false" $> False)

jsonBooleanParser :: Parser Json.Value
jsonBooleanParser = Json.Boolean <$> booleanParser

nullParser :: Parser ()
nullParser = void $ P.string "null"

jsonNullParser :: Parser Json.Value
jsonNullParser = nullParser $> Json.Null

jsonArrayParser :: Parser Json.Value
jsonArrayParser = P.char '[' *> (Json.Array <$> (whitespaceParser *> P.char ']' $> [] <|> arrayContentParser))
    where
        arrayContentParser :: Parser [Json.Value]
        arrayContentParser = do
            val <- jsonValueParser
            char <- P.anyChar
            case char of
                ']' -> return [val]
                ',' -> (val:) <$> arrayContentParser
                _ -> fail $ "expected ',' or ']' but found " ++ [char]

jsonObjectParser :: Parser Json.Value
jsonObjectParser = P.char '{' *> (Json.Object <$> (whitespaceParser *> P.char '}' $> M.empty <|> (M.fromList <$> objectContentParser)))
    where
        objectContentParser :: Parser [(Text, Json.Value)]
        objectContentParser = do
            _ <- whitespaceParser
            key <- stringParser
            _ <- whitespaceParser >> P.char ':'
            val <- jsonValueParser
            char <- P.anyChar
            case char of
                '}' -> return [(key, val)]
                ',' -> ((key,val):) <$> objectContentParser
                _ -> fail $ "expected ',' or '}' but found " ++ [char]

jsonValueParser :: Parser Json.Value
jsonValueParser = whitespaceParser *> (
        jsonBooleanParser <|> jsonNullParser <|> jsonObjectParser <|> jsonArrayParser <|> jsonNumberParser <|> jsonStringParser
    ) <* whitespaceParser

jsonParser :: Parser Json.Value
jsonParser = jsonValueParser
