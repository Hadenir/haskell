module Main where

import Lib

main :: IO ()
main = do
    rules <- readRulesFile "rules.txt"
    transformFile "testfile.json" "output.json" rules
    putStrLn "Done!"
