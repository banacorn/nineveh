-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/url

module Value.Url (
    parseUrl
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type
import Value.String

parseUrl = url >>= return . Url