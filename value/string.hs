-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/string

module Value.String (
    parseString
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type

parseString = string' >>= return . String'