module ParserCommon
    ( nullParser
    , booleanParser
    , numberParser
    , stringParser
    ) where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T

nullParser :: Parser ()
nullParser = void $ A.string "null"

booleanParser :: Parser Bool
booleanParser = (A.string "true" $> True) <|> (A.string "false" $> False)

numberParser :: Parser Double
numberParser = A.double

stringParser :: Parser Text
stringParser = A.char '"' *> stringContentParser
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
