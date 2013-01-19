-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/number

module Value.Number (
    Number(..),
    number
    ) where

import Parser
import Text.ParserCombinators.Parsec

data Number = Number Double

instance Show Number where
    show (Number n) = show n

number = lexeme $ do 
        i <- integer
        return (Number $ fromIntegral i) 
    <|> do 
        f <- float
        return (Number f)
    <|> do 
        char '.'
        i <- integer
        return (Number $ pointN i)
        where   pointN n = ((fromIntegral n) *) . (1 /) . (10 ^) . succ . floor . logBase 10 $ (fromIntegral n)