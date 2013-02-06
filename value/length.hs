-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/length

module Value.Length (
    parseLength
    ) where

import Value.Type
import Value.Number

import Tokenize
import Text.ParserCombinators.Parsec


unit =  try (string "em" >> return Em)
    <|>     (string "ex" >> return Ex)
    <|> try (string "ch" >> return Ch)
    <|>     (string "cm" >> return Cm)
    <|>     (string "rem" >> return Rem)
    <|> try (string "vh" >> return Vh)
    <|> try (string "vw" >> return Vw)
    <|> try (string "vmin" >> return Vmin)
    <|>     (string "vmax" >> return Vmax)
    <|>     (string "mm" >> return Mm)
    <|>     (string "in" >> return In)
    <|> try (string "px" >> return Px)
    <|> try (string "pt" >> return Pt)
    <|>     (string "pc" >> return Pc)

parseLength = lexeme $ do
    Number n <- parseNumber
    ctor <- unit
    return (ctor n)