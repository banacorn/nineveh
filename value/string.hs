-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/string

module Value.String (
    parseString
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Type

parseString = lexeme $ do
        s <- between (char '\'') (char '\'') (many (noneOf "\'"))
        return (String' s)
    <|> do
        s <- between (char '\"') (char '\"') (many (noneOf "\""))
        return (String' s)
