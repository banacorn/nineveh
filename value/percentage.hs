-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/percentage

module Value.Percentage (
    Percentage(..),
    percentage
    ) where

import Value.Number

import Parser
import Text.ParserCombinators.Parsec

data Percentage = Percentage Number


instance Show Percentage where
    show (Percentage n) = show n ++ "%"

percentage = lexeme $ do
    n <- number
    char '%'
    return (Percentage n)
