-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/frequency

module Value.Frequency (
    parseFrequency
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Number



unit =  (string "Hz" >> return Hz)
    <|> (string "KHz" >> return KHz)

parseFrequency = do
    Number n <- parseNumber
    ctor <- unit
    return (ctor n)