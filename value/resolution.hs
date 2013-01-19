-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/resolution

module Value.Resolution (
    Resolution(..),
    resolution
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Number

data Resolution = Dpi Number
                | Dpcm Number
                | Dppx Number

instance Show Resolution where
    show (Dpi n) = show n ++ "dpi"
    show (Dpcm n) = show n ++ "dpcm"
    show (Dppx n) = show n ++ "dppx"


unit =  try (string "dpi" >> return Dpi)
    <|> try (string "dpcm" >> return Dpcm)
    <|> (string "dppx" >> return Dppx)

resolution = lexeme $ do
    n <- number
    ctor <- unit
    return (ctor n)