-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/timing-function

module Value.TimingFunction (
    TimingFunction(..),
    timingFunction
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Integer
import Value.Number

data TimingFunctionDirection = Start | End

data TimingFunction = CubicBezier (Number, Number, Number, Number)
                    | Steps (Integer, TimingFunctionDirection)

instance Show TimingFunctionDirection where
    show Start = "start"
    show End = "end"

instance Show TimingFunction where
    show (CubicBezier (x1, y1, x2, y2)) = "cubic-bezier(" ++ show x1 ++ ", " ++ show y1 ++ ", " ++ show x2 ++ ", " ++ show y2 ++ ")"
    show (Steps (n, direction)) = "steps(" ++ show n ++ ", " ++ show direction ++ ")"



direction   =   (symbol "start" >> return Start) 
            <|> (symbol "end" >> return End)

cubicBezier = lexeme $ do
    string "cubic-bezier"
    [x1, y1, x2, y2] <- parens (number `sepBy` symbol ",")
    return (CubicBezier (x1, y1, x2, y2))

steps = lexeme $ do
    string "steps("
    n <- integer'
    symbol ","
    d <- direction
    return (Steps (n, d))

timingFunction = cubicBezier <|> steps


