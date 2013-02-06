-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/percentage

module Value.Percentage (
    parsePercentage
    ) where

import Value.Type
import Value.Number

import Tokenize
import Text.ParserCombinators.Parsec


parsePercentage = do
    Number n <- parseNumber
    char '%'
    return (Percentage n)
