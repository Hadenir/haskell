{-# OPTIONS -Wall -Wextra #-}

module JsonParser where

import System.Environment
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Char

type Parser a = StateT String Maybe a

parseChar :: Parser Char
parseChar = do
    (x:xs) <- get
    put xs
    return x

previewChar :: Parser Char
previewChar = do
    (x:_) <- get
    return x

failure :: Parser a
failure = StateT $ const Nothing

parse :: Parser a -> String -> Maybe a
parse parser input = fst <$> runStateT parser input

parseEnsureConsumeAll :: Parser a -> String -> Maybe a
parseEnsureConsumeAll parser input = case runStateT parser input of
    Just (v, "") -> Just v
    _ -> Nothing

ensureChar :: Char -> Parser Char
ensureChar c = do
    d <- parseChar
    if d == c then
        return d
    else
        failure

ensureMatching :: [Char] -> Parser Char
ensureMatching set = do
    d <- parseChar
    if d `elem` set then
        return d
    else
        failure

ensureMatchingIf :: (Char -> Bool) -> Parser Char
ensureMatchingIf func = do
    d <- parseChar
    if func d then
        return d
    else
        failure

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
        failure

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
                _ -> failure
        
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