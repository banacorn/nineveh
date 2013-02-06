
module Value.Variable (
    parseVariable
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type

parseVariable = lexeme $ do
    char '@'
    i <- identifier
    return (Variable i)