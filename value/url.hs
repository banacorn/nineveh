-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/url

module Value.Url (
    Url(..),
    url
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.String

data Url = Url String

instance Show Url where
    show (Url s) = "url('" ++ s ++ "')"

literal = do
        String' s <- string'
        return s
    <|> many (noneOf ")")
        

url = lexeme $ do
    string "url"
    s <- parens literal
    return (Url s)