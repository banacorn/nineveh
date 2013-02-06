-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/number

module Value.Number (
    parseNumber
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type


parseNumber :: Parser Value
parseNumber = do
    n <- number
    case head n of
        '.' -> return . Number . read $ '0':n
        _   -> return . Number . read $ n