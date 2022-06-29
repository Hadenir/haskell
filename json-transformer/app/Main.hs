module Main where

import Lib
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) $ error "Invalid number of arguments"

    let (inputPath:rulesPath:outputPath:_) = args
    rules <- readRulesFile rulesPath
    transformFile inputPath outputPath rules
    putStrLn "Done!"
