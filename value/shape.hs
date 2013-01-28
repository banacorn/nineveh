-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/shape

module Value.Shape (
    parseShape
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Length

parseShape = lexeme $ do
    string "rect"
    [top, right, bottom, left] <- parens (parseLength `sepBy` symbol ",")
    return (Shape (top, right, bottom, left))