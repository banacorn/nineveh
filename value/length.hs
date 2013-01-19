-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/length

module Value.Length (
    Length(..),
    length'
    ) where

import Value.Number

import Parser
import Text.ParserCombinators.Parsec

data Length = Em Number
            | Ex Number
            | Ch Number
            | Rem Number
            | Vh Number
            | Vw Number
            | Vmin Number
            | Vmax Number
            | Px Number
            | Mm Number
            | Cm Number
            | In Number
            | Pt Number
            | Pc Number

instance Show Length where
    show (Em n) = show n ++ "em"
    show (Ex n) = show n ++ "ex"
    show (Ch n) = show n ++ "ch"
    show (Rem n) = show n ++ "rem"
    show (Vh n) = show n ++ "vh"
    show (Vw n) = show n ++ "vw"
    show (Vmin n) = show n ++ "vmin"
    show (Vmax n) = show n ++ "vmax"
    show (Px n) = show n ++ "px"
    show (Mm n) = show n ++ "mm"
    show (Cm n) = show n ++ "cm"
    show (In n) = show n ++ "in"
    show (Pt n) = show n ++ "pt"
    show (Pc n) = show n ++ "pc"

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

length' = lexeme $ do
    n <- number
    ctor <- unit
    return (ctor n)