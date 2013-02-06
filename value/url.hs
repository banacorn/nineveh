-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/url

module Value.Url (
    parseUrl
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type
import Value.String

literal = do
        String' s <- parseString
        return s
    <|> many (noneOf ")")
        

parseUrl = lexeme $ do
    string "url"
    s <- parens literal
    return (Url s)