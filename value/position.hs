-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/position

module Value.Position (
    Position(..),
    position
    ) where

import Parser
import Text.ParserCombinators.Parsec

data Position = Static | Relative | Absolute | Fixed | InheritPosition

instance Show Position where
    show Static = "static"
    show Relative = "relative"
    show Absolute = "absolute"
    show Fixed = "fixed"
    show InheritPosition = "inherit"

position =  (symbol "static" >> return Static)
        <|> (symbol "relative" >> return Relative)
        <|> (symbol "absolute" >> return Absolute)
        <|> (symbol "fixed" >> return Fixed)
        <|> (symbol "inherit" >> return InheritPosition)