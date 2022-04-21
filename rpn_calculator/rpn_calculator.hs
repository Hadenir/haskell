{-# OPTIONS -Wall -Wextra #-}

module RpnCalculator where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Except
import Text.Read (readMaybe)
import Data.Maybe

type Context a = ExceptT String (State [Double]) a

push :: Double -> Context ()
push val = lift $ modify' (val:)

pop :: Context (Double, Double)
pop = do
    stack <- lift get
    case stack of
        (x:y:xs) -> lift $ put xs >> return (x, y)
        _ -> throwE "Two values required on the stack"

readOp :: String -> Maybe (Double -> Double -> Double)
readOp "+" = Just (+)
readOp "-" = Just (-)
readOp "*" = Just (*)
readOp "/" = Just (/)
readOp _ = Nothing

rpnStep :: String -> Context ()
rpnStep string = fromMaybe (throwE "Cannot parse input")
    (push <$> readMaybe string <|> (\x -> pop >>= push . uncurry x) <$> readOp string)

extractResult :: Context Double
extractResult = do
  stack <- lift get
  case stack of
    [x] -> return x
    _ -> throwE "Stack contains more or less than one element"

calculateRpn :: [String] -> Either String Double
calculateRpn list = evalState (runExceptT (mapM_ rpnStep list >> extractResult)) []