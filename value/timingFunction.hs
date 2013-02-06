-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/timing-function

module Value.TimingFunction (
    parseTimingFunction
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Integer
import Value.Number



direction   =   (symbol "start" >> return Start) 
            <|> (symbol "end" >> return End)

cubicBezier = lexeme $ do
    string "cubic-bezier"
    [Number x1, Number y1, Number x2, Number y2] <- parens (parseNumber `sepBy` symbol ",")
    return (CubicBezier (x1, y1, x2, y2))

steps = lexeme $ do
    string "steps("
    Integer' n <- parseInteger
    symbol ","
    d <- direction
    return (Steps (n, d))

parseTimingFunction = cubicBezier <|> steps


