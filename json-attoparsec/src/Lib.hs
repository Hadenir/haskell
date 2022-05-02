module Lib
    ( someFunc
    ) where

import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.Text.Lazy.IO as L
import JsonParser

someFunc :: IO ()
someFunc = do
    text <- L.readFile "large-file.json"
    case P.parse jsonParser text of
        P.Fail {} -> putStrLn "Failed"
        P.Done _ json -> putStrLn ("Succeded:\n" ++ show json)
