-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/frequency

module Value.Frequency (
    parseFrequency
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Number



unit =  (string "Hz" >> return Hz)
    <|> (string "KHz" >> return KHz)

parseFrequency = lexeme $ do
    Number n <- parseNumber
    ctor <- unit
    return (ctor n)