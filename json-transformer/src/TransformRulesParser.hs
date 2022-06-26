module TransformRulesParser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import JsonPath
import TransformRules
import ParserCommon

whitespaceParser :: Parser ()
whitespaceParser = A.skipWhile isWhitespace
    where
        isWhitespace c = case c of
            ' ' -> True
            '\n' -> True
            '\r' -> True
            '\t' -> True
            _ -> False

pathRootParser :: Parser JsonPath
pathRootParser = do
    A.char '$'
    RootPath <$> jsonPathParser

pathMemberParser :: Parser JsonPath
pathMemberParser = do
    A.char '.'
    member <- A.takeWhile $ A.notInClass ".[="
    ObjectPath member <$> jsonPathParser

pathElementParser :: Parser JsonPath
pathElementParser = do
    A.char '['
    index <- A.decimal
    A.char ']'
    ArrayPath (fromIntegral index) <$> jsonPathParser

jsonPathParser :: Parser JsonPath
jsonPathParser = pathMemberParser <|> pathElementParser <|> return NilPath

transformNullParser :: Parser (JsonPath -> TransformRule)
transformNullParser = nullParser $> SetValueNull

transformBoolParser :: Parser (JsonPath -> TransformRule)
transformBoolParser = SetValueBool <$> booleanParser

transformStringParser :: Parser (JsonPath -> TransformRule)
transformStringParser = SetValueString <$> stringParser

transformNumberParser :: Parser (JsonPath -> TransformRule)
transformNumberParser = SetValueNumber <$> numberParser

transformRuleParser :: Parser TransformRule
transformRuleParser = do
    path <- pathRootParser
    whitespaceParser *> A.char '=' <* whitespaceParser
    transform <- transformNullParser <|> transformBoolParser <|> transformStringParser <|> transformNumberParser
    return $ transform path

transformRulesParser :: Parser TransformRules
transformRulesParser = do
    rule <- whitespaceParser *> transformRuleParser <* whitespaceParser
    (rule:) <$> transformRulesParser <|> return [rule]
