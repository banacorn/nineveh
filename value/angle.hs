-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/angle

module Value.Angle (
    parseAngle
    ) where


import Value.Type
import Value.Number

import Parser
import Text.ParserCombinators.Parsec

unit =  (string "deg" >> return Deg)
    <|> (string "grad" >> return Grad)
    <|> (string "rad" >> return Rad)
    <|> (string "turn" >> return Turn)

parseAngle = lexeme $ do
    Number n <- parseNumber
    ctor <- unit
    return (ctor n)