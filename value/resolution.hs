-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/resolution

module Value.Resolution (
    parseResolution
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type
import Value.Number


unit =  try (string "dpi" >> return Dpi)
    <|> try (string "dpcm" >> return Dpcm)
    <|> (string "dppx" >> return Dppx)

parseResolution = lexeme $ do
    Number n <- parseNumber
    ctor <- unit
    return (ctor n)