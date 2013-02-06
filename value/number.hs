-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/number

module Value.Number (
    parseNumber
    ) where

import Tokenize
import Text.ParserCombinators.Parsec

import Value.Type


parseNumber = lexeme $ try (do 
        f <- float
        return (Number f))
    <|> do 
        i <- integer
        return (Number $ fromIntegral i) 
    <|> do 
        char '.'
        i <- integer
        return (Number $ pointN i)
        where   pointN n = ((fromIntegral n) *) . (1 /) . (10 ^) . succ . floor . logBase 10 $ (fromIntegral n)