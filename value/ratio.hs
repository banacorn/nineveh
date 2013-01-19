-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/ratio

module Value.Ratio (
    Ratio(..),
    ratio
    ) where

import Parser
import Text.ParserCombinators.Parsec

data Ratio = Ratio Integer Integer

instance Show Ratio where
    show (Ratio n d) = show n ++ "/" ++ show d

ratio = lexeme $ do
    n <- natural
    char '/'
    d <- natural
    return (Ratio n d)