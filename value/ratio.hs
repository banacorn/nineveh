-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/ratio

module Value.Ratio (
    parseRatio
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Number

parseRatio = lexeme $ do
    n <- natural
    char '/'
    d <- natural
    return (Ratio n d)