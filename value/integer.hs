-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/integer

module Value.Integer (
    Integer'(..),
    integer'
    ) where

import Parser
import Text.ParserCombinators.Parsec

data Integer' = Integer' Integer

integer' = integer