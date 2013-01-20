-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/string

module Value.String (
    String'(..),
    string'
    ) where

import Parser
import Text.ParserCombinators.Parsec

data String' = String' String

instance Show String' where
    show (String' s) = "\'" ++ s ++ "\'"

string' = lexeme $ do
        s <- between (char '\'') (char '\'') (many (noneOf "\'"))
        return (String' s)
    <|> do
        s <- between (char '\"') (char '\"') (many (noneOf "\""))
        return (String' s)
