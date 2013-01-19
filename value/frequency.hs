-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/frequency

module Value.Frequency (
    Frequency(..),
    frequency
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Number

data Frequency  = Hz Number
                | KHz Number

instance Show Frequency where
    show (Hz n) = show n ++ "Hz"
    show (KHz n) = show n ++ "KHz"


unit =  (string "Hz" >> return Hz)
    <|> (string "KHz" >> return KHz)

frequency = lexeme $ do
    n <- number
    ctor <- unit
    return (ctor n)