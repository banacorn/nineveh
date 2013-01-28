
module Value.Variable (
    parseVariable
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Type

parseVariable = lexeme $ do
    char '@'
    i <- identifier
    return (Variable i)