{-# OPTIONS -Wall -Wextra #-}

module CsvParser where

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

parseMatching :: [Char] -> Parser String
parseMatching = some . ensureMatching

parseInt :: Parser Integer
parseInt = read <$> parseMatching "1234567890-"

parseDouble :: Parser Double
parseDouble = read <$> parseMatching "0123456789.e-"

parseCsvLine :: Parser [Double]
parseCsvLine = skipWhitespaces >> (
    do
        i <- parseDouble
        rest <- (skipWhitespaces >> ensureChar ',' >> parseCsvLine) <|> return []
        return (i:rest)
    ) <|> return []
    where
        skipWhitespaces = eatCharsIf isSpace

parseCsv :: Parser [[Double]]
parseCsv = (
    do
        line <- parseCsvLine
        rest <- (ensureChar '\n' >> parseCsv) <|> return []
        return (line:rest)
    ) <|> return []

main :: IO ()
main = do
    args <- getArgs
    fileContent <- readFile $ head args
    print (parse parseCsv fileContent)