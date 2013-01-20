-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/time

module Value.Time (
    Time(..),
    time
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Number

data Time   = S Number
            | Ms Number

instance Show Time where
    show (S n) = show n ++ "s"
    show (Ms n) = show n ++ "ms"


unit =  (string "s" >> return S)
    <|> (string "ms" >> return Ms)

time = lexeme $ do
    n <- number
    ctor <- unit
    return (ctor n)