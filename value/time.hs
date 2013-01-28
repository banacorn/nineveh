-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/time

module Value.Time (
    parseTime
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Number

unit =  (string "s" >> return S)
    <|> (string "ms" >> return Ms)

parseTime = lexeme $ do
    Number n <- parseNumber
    ctor <- unit
    return (ctor n)