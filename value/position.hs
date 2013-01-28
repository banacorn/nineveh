-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/position

module Value.Position (
    parsePosition
    ) where


import Parser
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Number


parsePosition   =   (symbol "static" >> return Static)
                <|> (symbol "relative" >> return Relative)
                <|> (symbol "absolute" >> return Absolute)
                <|> (symbol "fixed" >> return Fixed)
                <|> (symbol "inherit" >> return Inherit)