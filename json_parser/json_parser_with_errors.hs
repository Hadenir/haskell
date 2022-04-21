{-# OPTIONS -Wall -Wextra #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JsonParserWithErrors where

import System.Environment
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Char

type ParserError = Either String
type Parser a = StateT String ParserError a

instance MonadFail ParserError where
    fail = Left

instance Alternative ParserError where
  empty = Left ""
  Right r <|> _ = Right r
  _ <|> Right r = Right r
  _ <|> Left l = Left l

instance MonadPlus ParserError where

parseChar :: Parser Char
parseChar = do
    (x:xs) <- get
    put xs
    return x

previewChar :: Parser Char
previewChar = do
    (x:_) <- get
    return x

failure :: String -> Parser a
failure = StateT . const . Left

parse :: Parser a -> String -> ParserError a
parse parser input = fst <$> runStateT parser input

parseEnsureConsumeAll :: Parser a -> String -> ParserError a
parseEnsureConsumeAll parser input = case runStateT parser input of
    Right (v, "") -> Right v
    _ -> Left "Parsing didn't consume whole string"

ensureChar :: Char -> Parser Char
ensureChar c = do
    d <- parseChar
    if d == c then
        return d
    else
        failure ("Expected \'" ++ show c ++ "\' but found \'" ++ show d ++ "\'")

ensureMatching :: [Char] -> Parser Char
ensureMatching set = do
    d <- parseChar
    if d `elem` set then
        return d
    else
        failure ("Expected one of \"" ++ set ++ "\" but found \'" ++ show d ++ "\'")

ensureMatchingIf :: (Char -> Bool) -> Parser Char
ensureMatchingIf func = do
    d <- parseChar
    if func d then
        return d
    else
        failure ("Unexpected character found \'" ++ show d ++ "\'")

eatChars :: [Char] -> Parser ()
eatChars = void . many . ensureMatching

eatCharsIf :: (Char -> Bool) -> Parser ()
eatCharsIf = void . many . ensureMatchingIf

skipWhitespaces :: Parser ()
skipWhitespaces = eatCharsIf isSpace

ensurePrefix :: String -> Parser String
ensurePrefix [] = return []
ensurePrefix (c:cs) = do
    d <- parseChar
    if d == c then do
        ds <- ensurePrefix cs
        return (d:ds)
    else
        failure ("Expected \'" ++ show c ++ "\' but found \'" ++ show d ++ "\'")

parseMatching :: [Char] -> Parser String
parseMatching = some . ensureMatching

parseMatchingIf :: (Char -> Bool) -> Parser String
parseMatchingIf = many . ensureMatchingIf

data JsonValue =
    JsonObject [(String, JsonValue)]
    | JsonArray [JsonValue]
    | JsonNumber Double
    | JsonString String
    | JsonBool Bool
    | JsonNull
    deriving Show

parseJsonNull :: Parser JsonValue
parseJsonNull = ensurePrefix "null" >> return JsonNull

parseJsonBool :: Parser JsonValue
parseJsonBool = (ensurePrefix "true" >> return (JsonBool True)) <|> (ensurePrefix "false" >> return (JsonBool False))

parseJsonString :: Parser JsonValue
parseJsonString = do
    _ <- ensureChar '"'
    string <- parseJsonStringContent <|> return []
    _ <- ensureChar '"'
    return $ JsonString string
    where
        parseJsonStringContent :: Parser String
        parseJsonStringContent = do
            str1 <- parseMatchingIf $ not . flip elem "\"\\"
            c <- previewChar
            case c of
                '"' -> return str1
                '\\' -> do
                    _ <- parseChar
                    esc <- parseJsonStringEscape
                    str2 <- parseJsonStringContent
                    return $ str1 ++ (esc:str2)
                _ -> failure ("Unexpected character found \'" ++ show c ++ "\'")

        parseJsonStringEscape :: Parser Char
        parseJsonStringEscape = parseChar >>= \c -> return $ case c of
            't' -> '\t'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            _ -> c

parseJsonNumber :: Parser JsonValue
parseJsonNumber = JsonNumber . read <$> parseMatching "1234567890-.eE"

parseJsonArray :: Parser JsonValue
parseJsonArray = do
    _ <- ensureChar '['
    values <- parseJsonArrayContent
    _ <- skipWhitespaces >> ensureChar ']'
    return $ JsonArray values
    where
        parseJsonArrayContent :: Parser [JsonValue]
        parseJsonArrayContent = (
            do
                value <- parseJsonValue
                rest <- (skipWhitespaces >> ensureChar ',' >> parseJsonArrayContent) <|> return []
                return (value:rest)
            ) <|> return []

parseJsonObject :: Parser JsonValue
parseJsonObject = do
    _ <- ensureChar '{'
    object <- parseJsonObjectContent
    _ <- skipWhitespaces >> ensureChar '}'
    return $ JsonObject object
    where
        parseJsonObjectContent :: Parser [(String, JsonValue)]
        parseJsonObjectContent = (
            do
                JsonString key <- skipWhitespaces >> parseJsonString
                _ <- skipWhitespaces >> ensureChar ':'
                value <- parseJsonValue
                rest <- (skipWhitespaces >> ensureChar ',' >> parseJsonObjectContent) <|> return []
                return ((key, value):rest)
            ) <|> return []

parseJsonValue :: Parser JsonValue
parseJsonValue = skipWhitespaces >>
    (parseJsonNull <|> parseJsonBool <|> parseJsonString <|> parseJsonNumber <|> parseJsonArray <|> parseJsonObject)

main :: IO ()
main = do
    args <- getArgs
    fileContent <- readFile $ head args
    print (parse parseJsonValue fileContent)