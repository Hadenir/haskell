module Lib
    ( someFunc
    ) where

import Control.Monad
import Pipes
import qualified Pipes.Prelude as P
import System.IO

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

someFunc :: IO ()
someFunc = print pp

pp :: Text
pp = case A.parse (runEffect $ parseList >-> P.stdoutLn) "" of
        A.Fail {} -> "Fail"
        A.Done _ dbl -> "Great!"


parseDouble :: Parser Double
parseDouble = A.double

parseWS :: Parser ()
parseWS = void $ A.takeWhile isWhitespace
    where
        isWhitespace c = case c of
            ' ' -> True
            '\n' -> True
            '\r' -> True
            '\t' -> True
            _ -> False

parseList :: Producer String Parser ()
parseList = do
    _ <- lift parseWS
    num <- lift parseDouble
    yield $ show num
    parseList
