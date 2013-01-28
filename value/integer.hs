-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/integer

module Value.Integer (
    parseInteger
    ) where

import Control.Applicative

import Parser
import Text.ParserCombinators.Parsec

import Value.Type

parseInteger = Integer' <$> integer